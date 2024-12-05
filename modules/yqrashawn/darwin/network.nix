{ config, lib, pkgs, ... }:

{
  networking = {
    knownNetworkServices = [
      "Wi-Fi"
      "Thunderbolt Bridge"
      "AdGuard"
      # "Tailscale Tunnel"
      "iPhone USB"
      "USB 10/100/1000 LAN"
      # "LG Monitor Controls"
    ];
    # search = [ config.user.name ];
    dns = [
      # "2408:8207:2532:2340:52eb:f6ff:fe34:b910"
      "192.18.0.2"
      # "2a00:5a60::ad1:0ff"
      # "117.50.10.10"
      # "52.80.52.52"
      # "119.29.29.29"
      # "223.5.5.5"
      # "223.6.6.6"
    ];
  };

}
