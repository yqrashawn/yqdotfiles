{ config, lib, pkgs, ... }:

{
  launchd.user.agents.miniser-local-jar = {
    serviceConfig = {
      # Disabled = true;
      Label = "com.yqrashawn.miniser-local-jar";
      EnvironmentVariables = {
        http_proxy = "http://127.0.0.1:6152";
        https_proxy = "http://127.0.0.1:6152";
        all_proxy = "socks5://127.0.0.1:6153";
        HTTP_PROXY = "http://127.0.0.1:6152";
        HTTPS_PROXY = "http://127.0.0.1:6152";
        ALL_PROXY = "socks5://127.0.0.1:6153";
      };
      ProgramArguments = [
        "/bin/sh"
        "-c"
        ''
          mkdir -p ~/miniser
          cd ~/miniser
          java -version
          /bin/cp ~/workspace/home/miniser/.env.local ~/miniser/.env
          SOCKET_PORT=61941 PORT=61942 PROD=true HOSTNAME=$(hostname) exec -a miniser-local-jar ${pkgs.direnv}/bin/direnv exec ./ java --add-opens=java.base/java.nio=ALL-UNNAMED --add-opens=java.base/sun.nio.ch=ALL-UNNAMED -XX:MaxDirectMemorySize=1024m -Xmx8G -jar /Users/${config.user.name}/Library/CloudStorage/Dropbox/sync/miniser/miniser-standalone.jar
        ''
      ];
      RunAtLoad = true;
      KeepAlive = true;
      WatchPaths = [
        "/Users/${config.user.name}/Library/CloudStorage/Dropbox/sync/miniser/miniser-standalone.jar"
      ];
      StandardErrorPath = "/tmp/miniser-stderr.log";
      StandardOutPath = "/tmp/miniser-stdout.log";
    };
  };
}
