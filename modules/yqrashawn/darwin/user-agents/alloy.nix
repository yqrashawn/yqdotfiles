{
  config,
  lib,
  pkgs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  configFile = "${homeDir}/Library/CloudStorage/Dropbox/sync/local-grafana/alloy/config.alloy";
in
{
  launchd.user.agents.alloy = {
    # port at 12345
    serviceConfig = {
      Label = "com.yqrashawn.alloy";
      ProgramArguments = [
        "${pkgs.grafana-alloy}/bin/alloy"
        "run"
        configFile
        "--storage.path=${homeDir}/.local/share/alloy"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/alloy-stderr.log";
      StandardOutPath = "/tmp/alloy-stdout.log";
    };
  };
}
