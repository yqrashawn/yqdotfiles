{ config, lib, pkgs, inputs, ... }:

{
  launchd.user.agents.atuin-daemon = {
    serviceConfig = {
      Label = "com.yqrashawn.atuin-daemon";
      ProgramArguments = [
        "${inputs.atuin.packages.${pkgs.system}.default}/bin/atuin"
        "daemon"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/atuin-daemon-stderr.log";
      StandardOutPath = "/tmp/atuin-daemon-stdout.log";
    };
  };
}
