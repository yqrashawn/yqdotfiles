{ config, lib, pkgs, ... }:

{
  launchd.daemons.tailscaled = {
    serviceConfig = {
      Label = "com.tailscale.tailscaled";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path ${pkgs.tailscale} &amp;&amp; ${pkgs.tailscale}/bin/tailscaled -tun utun -statedir /Users/${config.user.name}/tailscale"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/tailscaled-stderr.log";
      StandardOutPath = "/tmp/tailscaled-stdout.log";
    };
  };
}
