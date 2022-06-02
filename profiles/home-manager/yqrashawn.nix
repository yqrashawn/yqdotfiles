{ config, lib, pkgs, ... }: {
  programs.git = {
    userEmail = "namy.19@gmail.com";
    userName = "yqrashawn";
    signing = {
      key = "namy.19@gmail.com";
      signByDefault = true;
    };
  };
}
