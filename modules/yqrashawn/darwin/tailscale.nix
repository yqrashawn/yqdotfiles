{ config, lib, pkgs, ... }:

{
  services.tailscale = {
    enable = true;
    package = pkgs.tailscale;
    magicDNS = { enable = true; };
    domain = "donkey-clownfish.ts.net";
  };
}
