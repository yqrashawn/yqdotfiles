{ config, lib, pkgs, ... }: {
  imports = [
    ./darwin/daemons/tailscale.nix
    ./darwin/user-agents/miniser-local-jar.nix
    ./darwin/user-agents/pextser.nix
    ./darwin/daemons/adguard-home-with-proxy.nix
  ];
}
