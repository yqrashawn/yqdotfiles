{ config, lib, pkgs, ... }: {
  user.name = "holybasil";
  hm = { imports = [ ./home-manager/holybasil.nix ]; };
  system.stateVersion = 5;
  system.defaults.dock.orientation = lib.mkForce "right";
}
