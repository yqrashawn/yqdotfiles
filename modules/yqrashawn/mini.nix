{
  config,
  lib,
  pkgs,
  ...
}:
{
  networking.hostName = "mini";

  imports = [
    ./darwin/daemons/tailscale.nix
    ./darwin/daemons/kill-bootpd.nix
    ./darwin/daemons/adguard-home-with-proxy.nix
    ./darwin/user-agents/restart-miniser-container.nix
    ./darwin/user-agents/bind-nextdns-ip.nix
    ./darwin/user-agents/atuin-server.nix
    ./darwin/user-agents/buildkite-agent.nix
    # ./darwin/openclaw.nix
  ];
}
