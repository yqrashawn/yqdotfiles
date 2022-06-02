{ config, lib, pkgs, ... }:

{
  launchd.daemons = {
    limit-maxfiles = {
      script = "/bin/launchctl limit maxfiles 524288 16777216";
      serviceConfig.RunAtLoad = true;
      serviceConfig.KeepAlive = false;
    };
    limit-maxproc = {
      script = "/bin/launchctl limit maxproc 16704 16704";
      serviceConfig.RunAtLoad = true;
      serviceConfig.KeepAlive = false;
    };
    sysctl = {
      script =
        "/usr/sbin/sysctl kern.maxfiles=16777216 kern.maxfilesperproc=16704";
      serviceConfig.RunAtLoad = true;
      serviceConfig.KeepAlive = false;
      serviceConfig.LaunchOnlyOnce = true;
    };
  };
}
