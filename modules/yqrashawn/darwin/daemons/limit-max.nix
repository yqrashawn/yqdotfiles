{ config, lib, pkgs, ... }:

{
  launchd.daemons = {
    limit-maxfiles = {
      command = "/bin/launchctl limit maxfiles 524288 16777216";
      # script = "/bin/launchctl limit maxfiles 524288 16777216";
      serviceConfig = {
        # Program = "/bin/launchctl";
        # ProgramArguments = [ "limit" "maxfiles" "524288" "16777216" ];
        # ProgramArguments = [ "limit" "maxfiles 524288 16777216" ];
        RunAtLoad = true;
        KeepAlive = false;
        # LaunchOnlyOnce = true;
        StandardOutPath = "/tmp/nix-custom-log.out.log";
        StandardErrorPath = "/tmp/nix-custom-log.error.log";
      };

    };
    limit-maxproc = {
      command = "/bin/launchctl limit maxproc 16704 16704";
      # script = "/bin/launchctl limit maxproc 16704 16704";
      serviceConfig = {
        # Program = "/bin/launchctl";
        # ProgramArguments = [ "limit" "maxproc" "16704" "16704" ];
        # ProgramArguments = [ "limit" "maxproc 16704 16704" ];
        RunAtLoad = true;
        KeepAlive = false;
        # LaunchOnlyOnce = true;
        StandardOutPath = "/tmp/nix-custom-log.out.log";
        StandardErrorPath = "/tmp/nix-custom-log.error.log";
      };
    };
    sysctl = {
      serviceConfig = {
        Program = "/usr/sbin/sysctl";
        ProgramArguments =
          [ "kern.maxfiles=16777216" "kern.maxfilesperproc=16704" ];
        RunAtLoad = true;
        KeepAlive = false;
        # LaunchOnlyOnce = true;
        StandardOutPath = "/tmp/nix-custom-log.out.log";
        StandardErrorPath = "/tmp/nix-custom-log.error.log";
      };
    };
  };
}
