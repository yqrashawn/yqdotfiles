{ config, lib, pkgs, ... }:

{
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
