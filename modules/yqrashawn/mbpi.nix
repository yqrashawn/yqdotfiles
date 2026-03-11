{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./darwin/daemons/tailscale-utun.nix
  ];

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

  # No GUI casks for server use, keep CLI brews
  homebrew.casks = lib.mkForce [];
}
