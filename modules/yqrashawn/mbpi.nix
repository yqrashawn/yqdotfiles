{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./darwin/daemons/tailscale-utun.nix
    ./darwin/user-agents/buildkite-agent.nix
  ];

  networking.hostName = "mbpi";

  # mbpi has newer Nix install with GID 350 for nixbld group
  ids.gids.nixbld = 350;

  # Disable non-essential agents for headless server
  launchd.user.agents.dark-light = {
    command = lib.mkForce "/usr/bin/true";
    serviceConfig.KeepAlive = lib.mkForce false;
    serviceConfig.RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.syncmail.serviceConfig = {
    Program = lib.mkForce "/usr/bin/true";
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.atuin-daemon.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.redis.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.postgresql.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.grafana.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.loki.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.prometheus.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.alloy.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  launchd.user.agents.tempo.serviceConfig = {
    ProgramArguments = lib.mkForce [ "/usr/bin/true" ];
    KeepAlive = lib.mkForce false;
    RunAtLoad = lib.mkForce false;
  };

  # No GUI casks for server use, keep CLI brews
  homebrew.casks = lib.mkForce [];

  # No fonts needed for headless server
  fonts.packages = lib.mkForce [];
}
