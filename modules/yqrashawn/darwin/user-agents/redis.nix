{
  config,
  lib,
  pkgs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  dataDir = "${homeDir}/.local/share/redis";

  redisConf = pkgs.writeText "redis.conf" ''
    bind 127.0.0.1 ::1
    port 6379
    daemonize no
    databases 16
    dir ${dataDir}
    dbfilename dump.rdb
    loglevel notice
    protected-mode yes
    tcp-keepalive 300
    stop-writes-on-bgsave-error yes
    rdbcompression yes
    rdbchecksum yes
  '';
in
{
  launchd.user.agents.redis = {
    serviceConfig = {
      Label = "com.yqrashawn.redis";
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "mkdir -p ${dataDir} && exec ${pkgs.redis}/bin/redis-server ${redisConf}"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/redis-stderr.log";
      StandardOutPath = "/tmp/redis-stdout.log";
    };
  };
}
