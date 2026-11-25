{ config, lib, pkgs, ... }:

let
  homeDir = "/Users/${config.user.name}";
  hostname = config.networking.hostName;

  # Self-hosted GitHub Actions runners for 0gfoundation repos, named
  # <host>-<attr> (e.g. mini-zgh, studio-cchp-1).
  #
  # tokenFile holds a one-time registration token, not a PAT (the org does not
  # allow fine-grained PATs). It is only read when the runner is not yet
  # registered; afterwards the runner keeps its own credentials in
  # /var/lib/github-runners/<host>-<attr>/.runner. Nothing re-registers on config
  # changes: after changing url/name/labels, delete that .runner file, then
  # write a fresh token (the token file is watched, so every runner of that repo
  # on the host restarts, killing any job it is running):
  #   gh api -X POST repos/0gfoundation/<repo>/actions/runners/registration-token \
  #     -q .token | tr -d '\n' > ~/.config/github-runner/<repo>.token
  # One token registers any number of runners within its hour, so every runner
  # of a repo shares that repo's token file.
  mkRunner = repo: {
    enable = true;
    url = "https://github.com/0gfoundation/${repo}";
    tokenFile = "${homeDir}/.config/github-runner/${repo}.token";
    extraLabels = [ hostname ];
    replace = true;
    # Run as the main user, like buildkite-agent, so jobs get the user env.
    user = config.user.name;
    extraEnvironment.HOME = homeDir;
    # GitHub refuses outdated runners ("v2.330.0 is deprecated and cannot
    # receive messages"); the main nixpkgs pin lags, so take it from small.
    # small has no x86_64-darwin (nixpkgs 26.11 dropped it): mbpi takes 26.05's.
    package =
      if pkgs.stdenv.hostPlatform.isx86_64 then pkgs.darwin-intel.github-runner else pkgs.small.github-runner;
    # That package only ships node24; node20 actions (e.g. checkout@v4) fail.
    nodeRuntimes = [ "node24" ];
  };

  # cchp runners per host: the always-on desktops take two jobs at once, the
  # laptop one, and mbpi (Intel, 64 GB) three. Each runner is its own launchd
  # daemon and work dir, and cchp's `make test-unit-ci` keys its database on the
  # runner name, so two jobs on one host never share a database.
  cchpCount = { studio = 2; mini = 2; mbp = 1; mbpi = 3; }.${hostname} or 0;
  cchpRunners = lib.listToAttrs (map
    (i: lib.nameValuePair "cchp-${toString i}"
      ((mkRunner "cchp") // { name = "${hostname}-cchp-${toString i}"; }))
    (lib.range 1 cchpCount));

  # zgh runners per host: zgh's CI is 7 jobs, so two on mbp, one on mini.
  # studio takes none: its 32 GB swapped out with a zgh JVM next to the mydeck
  # agents. mbpi (Intel, 64 GB, otherwise idle) takes three. The first runner
  # keeps its old name (<host>-zgh) so it needs no re-registration; the rest
  # are <host>-zgh-<i>.
  zghCount = { mini = 1; mbp = 2; mbpi = 3; }.${hostname} or 0;
  zghAttrs = map (i: if i == 1 then "zgh" else "zgh-${toString i}") (lib.range 1 zghCount);

  # zgh's CI runs every step as `nix develop -c …`. nix develop's rc makes a
  # nix-shell.XXXXXX dir under $TMPDIR, points TMPDIR at it, and never deletes
  # it (nor its own nix-develop-* dir); zgh's tests fill it, and test servers
  # (valkey, the mcp fixture) outlive the job holding sockets in it. That grew
  # /tmp ~20 GB a day on mbp. So each zgh runner gets its own TMPDIR, and a
  # job-started hook clears the previous job's: a runner runs one job at a
  # time and no step has run yet, so every nix-shell.*/nix-develop-* in it, and
  # every process naming it in its arguments (both servers pass their socket
  # path there), is left over. Short: valkey sockets live under it and macOS
  # caps a socket path at 104 bytes. The runner picks a hook's shell by its
  # extension and refuses a path not ending in .sh/.ps1/.js.
  zghTmp = attr: "/private/tmp/ghr-${attr}";
  zghJobStarted = attr: pkgs.writeShellScript "github-runner-${attr}-job-started.sh" ''
    # The runner runs this as `bash -e <script>`, and a hook that exits
    # non-zero fails the job before its first step. Cleanup is best effort:
    # pkill exits 1 when nothing is left to match, find when $tmp is gone.
    set +e
    tmp=${zghTmp attr}
    if /usr/bin/pgrep -f "$tmp/" >/dev/null; then
      /usr/bin/pkill -f "$tmp/"
      for _ in 1 2 3 4 5; do /usr/bin/pgrep -f "$tmp/" >/dev/null || break; /bin/sleep 1; done
      /usr/bin/pkill -9 -f "$tmp/"
    fi
    /usr/bin/find "$tmp" -mindepth 1 -maxdepth 1 \( -name 'nix-shell.*' -o -name 'nix-develop-*' \) \
      -exec /bin/rm -rf {} + 2>/dev/null
    # periodic's clean-tmps may have removed an idle one
    /bin/mkdir -p -m 700 "$tmp"
    exit 0
  '';
  zghRunners = lib.listToAttrs (map
    (attr: lib.nameValuePair attr (lib.recursiveUpdate (mkRunner "zgh") {
      name = "${hostname}-${attr}";
      extraEnvironment = {
        TMPDIR = zghTmp attr;
        ACTIONS_RUNNER_HOOK_JOB_STARTED = "${zghJobStarted attr}";
      };
    }))
    zghAttrs);

  runners = zghRunners // cchpRunners;
in
{
  services.github-runners = runners;

  # User and system profiles provide the job tools, same as buildkite-agent.
  launchd.daemons = lib.mapAttrs'
    (attr: _: lib.nameValuePair "github-runner-${attr}" {
      path = lib.mkMerge [
        [
          "/etc/profiles/per-user/${config.user.name}/bin"
          "/run/current-system/sw/bin"
        ]
        # buildkite-agent also keeps the inherited $PATH, and with it the macOS
        # base tools; this list replaces PATH outright, so name them. Without
        # /bin a job has no ps, and zgh's process tests read ps. Last, so
        # Apple's bash 3.2 and git never shadow the runner module's own bash,
        # coreutils and git.
        (lib.mkAfter [
          "/usr/bin"
          "/bin"
          "/usr/sbin"
          "/sbin"
        ])
      ];
      # The runner's own TMPDIR must exist before Runner.Listener starts.
      script = lib.mkIf (lib.elem attr zghAttrs)
        (lib.mkBefore "/bin/mkdir -p -m 700 ${zghTmp attr}");
    })
    runners;
}
