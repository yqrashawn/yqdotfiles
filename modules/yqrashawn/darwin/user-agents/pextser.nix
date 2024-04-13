{ config, lib, pkgs, ... }:

{
  launchd.user.agents.pextser = {
    serviceConfig = {
      Label = "com.yqrashawn.pextser";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "cd /Users/${config.user.name}/Library/CloudStorage/Dropbox/sync/LOCAL-SERVER && ${pkgs.babashka}/bin/bb /Users/${config.user.name}/Library/CloudStorage/Dropbox/sync/LOCAL-SERVER/src/server.clj"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/pextser-stderr.log";
      StandardOutPath = "/tmp/pextser-stdout.log";
    };
  };
}
