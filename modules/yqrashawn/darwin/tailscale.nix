{ config, lib, pkgs, ... }:

{
  services.tailscale = {
    enable = false;
    package = pkgs.tailscale;
    magicDNS = { enable = true; };
    domain = "donkey-clownfish.ts.net";
  };
}
