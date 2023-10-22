{ config, lib, pkgs, ... }: {
  imports = [
    ./darwin/daemons/tailscale.nix
    ./darwin/daemons/adguard-home-with-proxy.nix
  ];
  programs.starship.settings.battery.disabled = false;
}
