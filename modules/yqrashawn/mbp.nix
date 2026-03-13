{
  config,
  lib,
  pkgs,
  ...
}:
{
  networking.hostName = "mbp";

  imports = [
    ./darwin/daemons/tailscale.nix
    # ./darwin/user-agents/miniser-local-jar.nix
    ./darwin/user-agents/pextser.nix
    ./darwin/user-agents/buildkite-agent.nix
    # ./darwin/daemons/adguard-home-with-proxy.nix
  ];
}
