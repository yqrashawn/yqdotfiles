{ config, lib, pkgs, ... }:

let
  homeDir = "/Users/${config.user.name}";
  hostname = config.networking.hostName;
in
{
  # Self-hosted GitHub Actions runner for 0gfoundation/zgh.
  #
  # tokenFile holds a one-time registration token, not a PAT (the org does not
  # allow fine-grained PATs). It is only read when the runner is not yet
  # registered; afterwards the runner keeps its own credentials in
  # /var/lib/github-runners/studio-zgh/.runner. Nothing re-registers on config
  # changes: after changing url/name/labels, delete that .runner file, then
  # write a fresh token (the token file is watched, so the daemon restarts):
  #   gh api -X POST repos/0gfoundation/zgh/actions/runners/registration-token \
  #     -q .token | tr -d '\n' > ~/.config/github-runner/zgh.token
  services.github-runners.zgh = {
    enable = true;
    url = "https://github.com/0gfoundation/zgh";
    name = "${hostname}-zgh";
    tokenFile = "${homeDir}/.config/github-runner/zgh.token";
    extraLabels = [ hostname ];
    replace = true;
    # Run as the main user, like buildkite-agent, so jobs get the user env.
    user = config.user.name;
    extraEnvironment.HOME = homeDir;
  };

  # User and system profiles provide the job tools, same as buildkite-agent.
  launchd.daemons.github-runner-zgh.path = [
    "/etc/profiles/per-user/${config.user.name}/bin"
    "/run/current-system/sw/bin"
  ];
}
