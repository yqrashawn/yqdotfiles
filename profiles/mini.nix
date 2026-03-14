{ config, lib, pkgs, ... }: {
  user.name = "yqrashawn";
  hm = { imports = [ ./home-manager/mini.nix ]; };
  system.stateVersion = 4;
}
