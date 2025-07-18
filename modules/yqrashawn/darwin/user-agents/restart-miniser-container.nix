{
  config,
  lib,
  pkgs,
  ...
}:

{
  launchd.user.agents.restart-miniser-container = {
    serviceConfig = {
      Label = "com.yqrashawn.restart-miniser-container";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        ''
          /usr/local/bin/docker restart miniser
        ''
      ];
      RunAtLoad = false;
      # KeepAlive = true;
      StartCalendarInterval = {
        Hour = 4;
        Minute = 1;
      };
      StandardErrorPath = "/tmp/restart-miniser-stderr.log";
      StandardOutPath = "/tmp/restart-miniser-stdout.log";
    };
  };
}
