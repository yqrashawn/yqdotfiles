{ config, lib, pkgs, ... }:

{
  programs.eza = {
    enable = true;
    enableAliases = true;
  };
}
