{
  config,
  lib,
  pkgs,
  ...
}:

{
  launchd.user.agents.mcp-prometheus = {
    # streamable-http at port 37902
    serviceConfig = {
      Label = "com.yqrashawn.mcp-prometheus";
      ProgramArguments = [
        "${pkgs.mcp-prometheus}/bin/mcp-prometheus"
        "serve"
        "--transport"
        "streamable-http"
        "--http-addr"
        "127.0.0.1:37902"
      ];
      EnvironmentVariables = {
        PROMETHEUS_URL = "http://localhost:9090";
      };
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/mcp-prometheus-stderr.log";
      StandardOutPath = "/tmp/mcp-prometheus-stdout.log";
    };
  };
}
