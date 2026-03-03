{
  config,
  lib,
  pkgs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  dataDir = "${homeDir}/.local/share/postgresql";

  icu78 = pkgs.icu78;

  # ParadeDB pg_search extension (pre-built binary from GitHub releases)
  # The .pkg binary links against Homebrew's icu4c@78.  The nix store paths
  # are too long for install_name_tool to rewrite in-place (Mach-O header
  # padding insufficient).  We create a symlink dir with the ICU 78 libs
  # and set DYLD_LIBRARY_PATH in the launch script.
  pg_search = pkgs.stdenv.mkDerivation rec {
    pname = "pg_search";
    version = "0.21.10";

    src = pkgs.fetchurl {
      url = "https://github.com/paradedb/paradedb/releases/download/v${version}/pg_search%4017--${version}.arm64_sequoia.pkg";
      name = "pg_search-17-${version}.arm64_sequoia.pkg";
      hash = "sha256-fea+PE9cVqTioT8Qywhq7EhdPN4D+gDF7XwoUSR9nbc=";
    };

    nativeBuildInputs = [ pkgs.cpio pkgs.gzip ];

    buildInputs = [ icu78 ];

    unpackPhase = ''
      ${pkgs.xar}/bin/xar -xf $src
      cat Payload | gunzip | cpio -id
    '';

    installPhase = ''
      mkdir -p $out/lib $out/share/postgresql/extension

      cp lib/postgresql/pg_search.dylib $out/lib/
      cp share/postgresql@17/extension/pg_search* $out/share/postgresql/extension/

      # Symlink ICU 78 libs for DYLD_LIBRARY_PATH resolution
      mkdir -p $out/icu-compat/lib
      ln -s ${icu78}/lib/libicuuc.78.dylib    $out/icu-compat/lib/libicuuc.78.dylib
      ln -s ${icu78}/lib/libicui18n.78.dylib  $out/icu-compat/lib/libicui18n.78.dylib
      ln -s ${icu78}/lib/libicudata.78.dylib  $out/icu-compat/lib/libicudata.78.dylib
    '';

    meta = with lib; {
      description = "Full text search for PostgreSQL using BM25 (ParadeDB)";
      homepage = "https://paradedb.com";
      license = licenses.agpl3Plus;
      platforms = [ "aarch64-darwin" ];
    };
  };

  # PostgreSQL with pgvector + pg_search extensions
  postgresqlWithExtensions = pkgs.postgresql.withPackages (ps: [
    ps.pgvector
    pg_search
  ]);

  # ICU compat lib path for pg_search runtime loading
  icuCompatLib = "${pg_search}/icu-compat/lib";
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
        # Set DYLD_LIBRARY_PATH in the shell to ensure pg_search.dylib can
        # find ICU 78 libs (launchd may strip DYLD_* from EnvironmentVariables).
        "export DYLD_LIBRARY_PATH='${icuCompatLib}'; [ -f ${dataDir}/PG_VERSION ] || ${postgresqlWithExtensions}/bin/initdb -D ${dataDir} && exec ${postgresqlWithExtensions}/bin/postgres -D ${dataDir} -c shared_preload_libraries=pg_search"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/postgresql-stderr.log";
      StandardOutPath = "/tmp/postgresql-stdout.log";
    };
  };
}
