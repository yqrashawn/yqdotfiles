{ config, lib, pkgs, ... }: {
  imports = [
    ./darwin/daemons/tailscale.nix
    ./darwin/daemons/adguard-home-with-proxy.nix
  ];
}
