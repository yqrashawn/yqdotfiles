{ config, lib, pkgs, ... }:

{
  programs.mcfly = {
    enable = true;
    keyScheme = "vim";
    fuzzySearchFactor = 3;
    enableZshIntegration = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };
}
