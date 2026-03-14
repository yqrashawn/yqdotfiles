{
  config,
  lib,
  pkgs,
  ...
}:

let
  homeDir = "/Users/${config.user.name}";
  buildDir = "${homeDir}/.local/share/buildkite-agent/builds";
  hooksDir = "${homeDir}/.local/share/buildkite-agent/hooks";
  pluginsDir = "${homeDir}/.local/share/buildkite-agent/plugins";
  tokenPath = config.sops.secrets.buildkite-agent-token.path;
  hostname = config.networking.hostName;

  buildkiteConfig = pkgs.writeText "buildkite-agent.cfg" ''
    name="${hostname}-%spawn"
    spawn=3
    priority=1
    tags="queue=default-queue,type=selfhost,system=macos,machine-label=${hostname}"
    build-path="${buildDir}"
    hooks-path="${hooksDir}"
    plugins-path="${pluginsDir}"
    shell="${buildkiteShell}"
  '';

  agentPath = "${pkgs.buildkite-agent}/bin";
  userPath = "/etc/profiles/per-user/${config.user.name}/bin";
  basePath = "${agentPath}:${userPath}:/run/current-system/sw/bin:$PATH";

  environmentHook = pkgs.writeShellScript "buildkite-environment-hook" ''
    export PATH="${basePath}"
  '';

  # Wrapper shell: sets PATH *before* exec-ing zsh, so even if /etc/zshenv
  # calls path_helper and rearranges PATH, buildkite-agent stays available.
  buildkiteShell = pkgs.writeShellScript "buildkite-shell-wrapper" ''
    export PATH="${basePath}"
    exec ${userPath}/zsh -e -c "$1"
  '';

  startScript = pkgs.writeShellScript "buildkite-agent-start" ''
    export BUILDKITE_AGENT_TOKEN="$(cat ${tokenPath})"
    mkdir -p ${buildDir} ${hooksDir} ${pluginsDir}
    ln -sf ${environmentHook} ${hooksDir}/environment
    exec ${pkgs.buildkite-agent}/bin/buildkite-agent start --config ${buildkiteConfig}
  '';
in
{
  sops.secrets.buildkite-agent-token = {
    owner = config.user.name;
  };

  launchd.user.agents.buildkite-agent = {
    serviceConfig = {
      Label = "com.yqrashawn.buildkite-agent";
      ProgramArguments = [ "${startScript}" ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/buildkite-agent-stderr.log";
      StandardOutPath = "/tmp/buildkite-agent-stdout.log";
      EnvironmentVariables = {
        PATH = "${pkgs.buildkite-agent}/bin:/etc/profiles/per-user/${config.user.name}/bin:/run/current-system/sw/bin:/usr/local/bin:/usr/bin:/bin";
      };
    };
  };
}
