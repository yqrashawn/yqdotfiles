{ config, lib, pkgs, ... }: {
  launchd.user.agents.syncmail = {
    serviceConfig = {
      # Disabled = true;
      Program = "${pkgs.babashka}/bin/bb";
      ProgramArguments = [ "-f" "${config.user.home}/local/bin/syncmail" ];
      StandardErrorPath = "/tmp/syncmail.error.log";
      StandardOutPath = "/tmp/syncmail.out.log";
      RunAtLoad = true;
      TimeOut = 600;
      ExitTimeOut = 600;
      StartInterval = 600;
    };
  };
}
