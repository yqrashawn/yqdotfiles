{ config, lib, pkgs, ... }: {
  networking.hostName = "studio";

  imports = [
    ./darwin/daemons/tailscale-utun.nix
    ./darwin/user-agents/pextser.nix
    ./darwin/user-agents/miniser-local-jar.nix
    ./darwin/user-agents/buildkite-agent.nix
  ];
}
