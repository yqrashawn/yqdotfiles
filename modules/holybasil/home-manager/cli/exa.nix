{ config, lib, pkgs, ... }:

{
  programs.exa = {
    enable = true;
    enableAliases = true;
  };
}
