{ config, lib, pkgs, ... }: {
  user.name = "yqrashawn";
  hm = { imports = [ ./home-manager/mbpi.nix ]; };
  system.stateVersion = 4;
}
