{
  config,
  lib,
  pkgs,
  ...
}:

{
  launchd.user.agents.bind-nextdns-ip = {
    serviceConfig = {
      Label = "com.yqrashawn.bind-nextdns-ip";
      ProgramArguments = [
        "/bin/sh"
        "/Users/${config.user.name}/Library/CloudStorage/Dropbox/sync/scripts/bind-nextdns-ip.sh"
      ];
      RunAtLoad = true;
      # KeepAlive = true;
      StartInterval = 20;
      StandardErrorPath = "/tmp/call-nextdns-stderr.log";
      StandardOutPath = "/tmp/call-nextdns-stdout.log";
    };
  };
}
