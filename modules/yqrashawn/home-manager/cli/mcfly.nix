{ config, lib, pkgs, ... }:

{
  programs.mcfly = {
    enable = true;
    keyScheme = "vim";
    fuzzySearchFactor = 3;
    enableZshIntegration = false;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };
}
