{ config, lib, pkgs, ... }:

let
  homeDir = "/Users/${config.user.name}";
  configFile =
    "${homeDir}/Library/CloudStorage/Dropbox/sync/local-grafana/grafana/grafana.ini";
in {
  launchd.user.agents.grafana = {
    serviceConfig = {
      Label = "com.yqrashawn.grafana";
      ProgramArguments = [
        "${pkgs.grafana}/bin/grafana-server"
        "--homepath=${pkgs.grafana}/share/grafana"
        "--config=${configFile}"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/grafana-stderr.log";
      StandardOutPath = "/tmp/grafana-stdout.log";
    };
  };
}
