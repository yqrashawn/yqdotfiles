{
  config,
  lib,
  pkgs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  dataDir = "${homeDir}/.local/share/postgresql";
in
{
  launchd.user.agents.postgresql = {
    # db postgres
    # user $USER
    # pwd none
    # port 5432
    serviceConfig = {
      Label = "com.yqrashawn.postgresql";
      EnvironmentVariables = {
        LC_ALL = "en_US.UTF-8";
      };
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "[ -f ${dataDir}/PG_VERSION ] || ${pkgs.postgresql}/bin/initdb -D ${dataDir} && exec ${pkgs.postgresql}/bin/postgres -D ${dataDir}"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/postgresql-stderr.log";
      StandardOutPath = "/tmp/postgresql-stdout.log";
    };
  };
}
