{ config, lib, pkgs, ... }: {
  imports = [
    ./dark-light.nix
    ./syncmail.nix
    ./atuin-daemon.nix
    ./grafana.nix
    ./loki.nix
    ./alloy.nix
  ];
}
