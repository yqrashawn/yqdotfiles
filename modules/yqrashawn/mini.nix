{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    # ./darwin/daemons/tailscale.nix
    ./darwin/daemons/kill-bootpd.nix
    ./darwin/daemons/adguard-home-with-proxy.nix
    ./darwin/user-agents/restart-miniser-container.nix
  ];
}
