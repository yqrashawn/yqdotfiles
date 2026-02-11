{
  config,
  lib,
  pkgs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  configFile = "${homeDir}/Library/CloudStorage/Dropbox/sync/local-grafana/prometheus.yml";
in
{
  launchd.user.agents.prometheus = {
    # port at 9090
    serviceConfig = {
      Label = "com.yqrashawn.prometheus";
      ProgramArguments = [
        "${pkgs.prometheus}/bin/prometheus"
        "--config.file=${configFile}"
        "--storage.tsdb.path=${homeDir}/.local/share/prometheus"
        "--web.listen-address=0.0.0.0:9090"
        "--web.enable-remote-write-receiver"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/prometheus-stderr.log";
      StandardOutPath = "/tmp/prometheus-stdout.log";
    };
  };
}
