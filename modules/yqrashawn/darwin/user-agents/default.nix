{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./dark-light.nix
    ./syncmail.nix
  ];
}
