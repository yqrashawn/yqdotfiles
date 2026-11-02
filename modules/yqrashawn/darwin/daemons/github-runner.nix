{ config, lib, pkgs, ... }:

let
  homeDir = "/Users/${config.user.name}";
  hostname = config.networking.hostName;

  # Self-hosted GitHub Actions runners, one per repo, named <host>-<attr>.
  #
  # tokenFile holds a one-time registration token, not a PAT (the org does not
  # allow fine-grained PATs). It is only read when the runner is not yet
  # registered; afterwards the runner keeps its own credentials in
  # /var/lib/github-runners/<host>-<attr>/.runner. Nothing re-registers on
  # config changes: after changing url/name/labels, delete that .runner file,
  # then write a fresh token (the token file is watched, so the daemon restarts):
  #   gh api -X POST repos/<owner>/<repo>/actions/runners/registration-token \
  #     -q .token | tr -d '\n' > ~/.config/github-runner/<attr>.token
  repos = {
    zgh = "https://github.com/0gfoundation/zgh";
    zgh-personal = "https://github.com/yqrashawn/zgh";
  };

  mkRunner = attr: url: {
    enable = true;
    inherit url;
    name = "${hostname}-${attr}";
    tokenFile = "${homeDir}/.config/github-runner/${attr}.token";
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
in
{
  services.github-runners = lib.mapAttrs mkRunner repos;

  # User and system profiles provide the job tools, same as buildkite-agent.
  launchd.daemons = lib.mapAttrs' (attr: _:
    lib.nameValuePair "github-runner-${attr}" {
      path = [
        "/etc/profiles/per-user/${config.user.name}/bin"
        "/run/current-system/sw/bin"
      ];
    }) repos;
}
