{ config, lib, pkgs, ... }: {
  launchd.user.agents.syncmail = {
    path = [ config.environment.systemPath ];
    serviceConfig = {
      # Disabled = true;
      Program = "${config.user.home}/.asdf/shims/bb";
      ProgramArguments = [
        "--file"
        "${config.user.home}/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/local-bins/syncmail"
      ];
      StandardErrorPath = "/tmp/syncmail.error.log";
      StandardOutPath = "/tmp/syncmail.out.log";
      RunAtLoad = true;
      TimeOut = 600;
      ExitTimeOut = 600;
      StartInterval = 600;
    };
  };
}
