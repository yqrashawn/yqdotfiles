{ config, lib, pkgs, ... }: {
  imports = [ ./cf-access.nix ./dark-light.nix ./syncmail.nix ];
}
