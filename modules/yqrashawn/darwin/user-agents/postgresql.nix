{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  dataDir = "${homeDir}/.local/share/postgresql";

  isAarch64 = pkgs.stdenv.hostPlatform.isAarch64;

  # PostgreSQL with pgvector + pg_search (pg_search only on aarch64).
  # pg_search requires Rust 1.90 toolchain via cargo-pgrx which has no
  # x86_64-darwin binary cache — impractical to build on Intel Macs.
  postgresqlWithExtensions = pkgs.postgresql.withPackages (ps:
    [ ps.pgvector ] ++ pkgs.lib.optionals isAarch64 [
      ((pkgs.callPackage "${inputs.paradedb-src}/nix/pg_search.nix" {
        cargo-pgrx = pkgs.cargo-pgrx_0_16_1;
        inherit (pkgs) postgresql;
      }).overrideAttrs (old: { meta = old.meta // { broken = false; }; }))
    ]
  );
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
        "[ -f ${dataDir}/PG_VERSION ] || ${postgresqlWithExtensions}/bin/initdb -D ${dataDir} && exec ${postgresqlWithExtensions}/bin/postgres -D ${dataDir}${pkgs.lib.optionalString isAarch64 " -c shared_preload_libraries=pg_search"}"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/postgresql-stderr.log";
      StandardOutPath = "/tmp/postgresql-stdout.log";
    };
  };
}
