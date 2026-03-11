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

  # ParadeDB pg_search extension (built from source via pgrx)
  # Uses ParadeDB's own nix build definition from their repo.
  # pgrx extensions are marked broken in nixpkgs because tests need a running
  # PostgreSQL in the sandbox; the built extension works fine.
  pg_search = (pkgs.callPackage "${inputs.paradedb-src}/nix/pg_search.nix" {
    cargo-pgrx = pkgs.cargo-pgrx_0_16_1;
    inherit (pkgs) postgresql;
  }).overrideAttrs (old: { meta = old.meta // { broken = false; }; });

  # PostgreSQL with pgvector + pg_search extensions
  postgresqlWithExtensions = pkgs.postgresql.withPackages (ps: [
    ps.pgvector
    pg_search
  ]);
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
        "[ -f ${dataDir}/PG_VERSION ] || ${postgresqlWithExtensions}/bin/initdb -D ${dataDir} && exec ${postgresqlWithExtensions}/bin/postgres -D ${dataDir} -c shared_preload_libraries=pg_search"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/postgresql-stderr.log";
      StandardOutPath = "/tmp/postgresql-stdout.log";
    };
  };
}
