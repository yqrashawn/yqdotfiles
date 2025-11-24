{ config, lib, pkgs, inputs, ... }:

{
  launchd.user.agents.atuin-server = {
    serviceConfig = {
      Label = "com.yqrashawn.atuin-server";
      ProgramArguments = [
        "${inputs.atuin.packages.${pkgs.system}.default}/bin/atuin"
        "server"
        "start"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/atuin-server-stderr.log";
      StandardOutPath = "/tmp/atuin-server-stdout.log";
    };
  };
}
