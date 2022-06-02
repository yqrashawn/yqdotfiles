{ config, lib, pkgs, ... }:

{
  programs.mcfly = {
    enable = true;
    keyScheme = "vim";
    enableFuzzySearch = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };
}
