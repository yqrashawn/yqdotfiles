{ config, lib, pkgs, ... }: {
  imports = [ ./set-path.nix ./limit-max.nix ./adguard-home.nix ];
}
