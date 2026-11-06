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
    package = pkgs.small.github-runner;
    # That package only ships node24; node20 actions (e.g. checkout@v4) fail.
    nodeRuntimes = [ "node24" ];
  };

  # cchp runners per host: the always-on desktops take two jobs at once, the
  # laptop one. Each runner is its own launchd daemon and work dir, and cchp's
  # `make test-unit-ci` keys its database on the runner name, so two jobs on
  # one host never share a database.
  cchpCount = { studio = 2; mini = 2; mbp = 1; }.${hostname} or 0;
  cchpRunners = lib.listToAttrs (map
    (i: lib.nameValuePair "cchp-${toString i}"
      ((mkRunner "cchp") // { name = "${hostname}-cchp-${toString i}"; }))
    (lib.range 1 cchpCount));

  # studio takes no zgh CI: its 32 GB swapped out with a zgh JVM next to the
  # mydeck agents.
  zghRunners = lib.optionalAttrs (hostname != "studio") {
    zgh = (mkRunner "zgh") // { name = "${hostname}-zgh"; };
  };

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
    })
    runners;
}
