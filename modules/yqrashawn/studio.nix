{ config, lib, pkgs, ... }: {
  imports = [ ./darwin/daemons/tailscale-utun.nix ];
}
