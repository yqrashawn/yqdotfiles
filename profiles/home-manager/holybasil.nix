{ config, lib, pkgs, ... }: {
  programs.git = {
    userEmail = "holybasil.1128@gmail.com";
    userName = "holybasil";
    signing = {
      key = "holybasil.1128@gmail.com";
      signByDefault = true;
    };
  };
}
