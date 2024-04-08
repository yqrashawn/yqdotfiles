{ config, lib, pkgs, ... }:

{
  launchd.daemons.kill-bootpd = {
    serviceConfig = {
      Label = "com.yqrashawn.kill-bootpd";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "while ! nc -z localhost 67; do sleep 1; done && sudo launchctl unload -w /System/Library/LaunchDaemons/bootps.plist && bash /Users/${config.user.name}/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/local-bins/restart-surge"
      ];
      RunAtLoad = true;
      KeepAlive = false;
      StandardErrorPath = "/tmp/killbootpd-stderr.log";
      StandardOutPath = "/tmp/killbootpd-stdout.log";
    };
  };
}
