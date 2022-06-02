{ config, lib, pkgs, ... }:

{
  launchd.daemons.set-path = {
    script = ''
      export PATH=/run/current-system/sw/bin:$PATH
    '';
    serviceConfig.RunAtLoad = true;
    serviceConfig.KeepAlive = false;
  };
}
