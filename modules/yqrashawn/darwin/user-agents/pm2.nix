{ config, lib, pkgs, ... }:

let
  homeDir = "/Users/${config.user.name}";
  pm2Home = "${homeDir}/.pm2";

  # pm2 comes from `bun add -g pm2` (v7), not nixpkgs: the binary running
  # `resurrect` must match the major version of the live daemon, and
  # pkgs.nodePackages.pm2 is still v6. Pinning the bun path here keeps that
  # coupling reviewable instead of buried in a hand-written LaunchAgent.
  pm2Bin = "${homeDir}/.cache/.bun/bin/pm2";

  # Resurrected apps run node/python/go/java/babashka/uv toolchains. asdf shims
  # are used rather than versioned install dirs so a runtime upgrade cannot
  # silently break boot.
  agentPath = lib.concatStringsSep ":" [
    "${homeDir}/.asdf/shims"
    "${homeDir}/.cache/.bun/bin"
    "${homeDir}/.local/bin"
    "${homeDir}/local/bin"
    "/etc/profiles/per-user/${config.user.name}/bin"
    "/run/current-system/sw/bin"
    "${homeDir}/.nix-profile/bin"
    "/nix/var/nix/profiles/default/bin"
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/usr/sbin"
    "/sbin"
  ];

  # `pm2 resurrect` restarts every app in the dump, so running it against a
  # live daemon would bounce every process on each darwin-rebuild. Only
  # resurrect when no daemon owns the pid file.
  resurrect = pkgs.writeShellScript "pm2-resurrect" ''
    pidfile="${pm2Home}/pm2.pid"
    if [ -r "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; then
      echo "pm2 daemon already running ($(cat "$pidfile")), skipping resurrect"
      exit 0
    fi
    exec "${pm2Bin}" resurrect
  '';
in
{
  launchd.user.agents.pm2 = {
    serviceConfig = {
      Label = "com.yqrashawn.pm2";
      ProgramArguments = [ "${resurrect}" ];
      RunAtLoad = true;
      # One-shot: `resurrect` exits once the daemon has the dump loaded.
      KeepAlive = false;
      EnvironmentVariables = {
        PATH = agentPath;
        PM2_HOME = pm2Home;
      };
      StandardErrorPath = "/tmp/pm2-stderr.log";
      StandardOutPath = "/tmp/pm2-stdout.log";
    };
  };
}
