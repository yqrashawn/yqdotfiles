{ config, lib, pkgs, ... }: {
  imports =
    [ ./set-path.nix ./limit-max.nix ./tailscale.nix ./adguard-home.nix ];
}
