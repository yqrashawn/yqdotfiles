{ config, lib, pkgs, ... }: {
  user.name = "holybasil";
  hm = { imports = [ ./home-manager/holybasil.nix ]; };
}
