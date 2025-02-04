{ config, lib, pkgs, ... }:
let
  email = "holybasil.1128@gmail.com";
  username = "holybasil";
in {
  programs.git = {
    userEmail = email;
    userName = username;
    extraConfig.github.user = username;
    extraConfig.core.editor = lib.mkForce "code --wait";
  };
}
