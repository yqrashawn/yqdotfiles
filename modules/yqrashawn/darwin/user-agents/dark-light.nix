{ config, lib, pkgs, ... }:

let localBin = "${config.user.home}/local/bin";
in {
  launchd.user.agents.dark-light = {
    command =
      "${localBin}/dark-mode-notify ${localBin}/dark_light_mode_change.clj";
    serviceConfig = {
      KeepAlive = true;
      StandardErrorPath = "/tmp/dark-mode-notify-stderr.log";
      StandardOutPath = "/tmp/dark-mode-notify-stdout.log";
    };
  };
}
