{
  config,
  lib,
  pkgs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  configFile = "${homeDir}/Library/CloudStorage/Dropbox/sync/local-grafana/tempo/tempo.yaml";
in
{
  launchd.user.agents.tempo = {
    # http api at 3200, otlp grpc at 4320, otlp http at 4321
    serviceConfig = {
      Label = "com.yqrashawn.tempo";
      ProgramArguments = [
        "${pkgs.tempo}/bin/tempo"
        "-config.file=${configFile}"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/tempo-stderr.log";
      StandardOutPath = "/tmp/tempo-stdout.log";
    };
  };
}
