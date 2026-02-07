{ config, lib, pkgs, ... }:

let
  homeDir = "/Users/${config.user.name}";
  configFile =
    "${homeDir}/Library/CloudStorage/Dropbox/sync/local-grafana/loki-local-config.yaml";
in {
  launchd.user.agents.loki = {
    serviceConfig = {
      Label = "com.yqrashawn.loki";
      ProgramArguments =
        [ "${pkgs.grafana-loki}/bin/loki" "-config.file=${configFile}" ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/loki-stderr.log";
      StandardOutPath = "/tmp/loki-stdout.log";
    };
  };
}
