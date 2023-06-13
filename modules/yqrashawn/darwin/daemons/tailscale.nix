{ config, lib, pkgs, ... }:

{
  launchd.daemons.tailscaled = {
    serviceConfig = {
      Label = "com.tailscale.tailscaled";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path ${pkgs.tailscale} &amp;&amp; ${pkgs.tailscale}/bin/tailscaled -tun userspace-networking -statedir /Users/${config.user.name}/tailscale --socks5-server=localhost:6156 --outbound-http-proxy-listen=localhost:6156"
        # "/bin/wait4path ${pkgs.tailscale} &amp;&amp; ${pkgs.tailscale}/bin/tailscaled -tun utun0 -statedir /Users/${config.user.name}/tailscale -outbound-http-proxy-listen localhost:6152"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/tailscaled-stderr.log";
      StandardOutPath = "/tmp/tailscaled-stdout.log";
    };
  };
}
