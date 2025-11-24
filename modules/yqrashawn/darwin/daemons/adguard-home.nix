{
  config,
  lib,
  pkgs,
  ...
}:

{
  launchd.daemons.adguardhome = {
    serviceConfig = {
      Label = "com.adguard.adguardhome";
      # EnvironmentVariables = {
      #   http_proxy = "http://127.0.0.1:6152";
      #   https_proxy = "http://127.0.0.1:6152";
      #   all_proxy = "socks5://127.0.0.1:6153";
      #   HTTP_PROXY = "http://127.0.0.1:6152";
      #   HTTPS_PROXY = "http://127.0.0.1:6152";
      #   ALL_PROXY = "socks5://127.0.0.1:6153";
      # };
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path ${pkgs.adguardhome} && ${pkgs.adguardhome}/bin/adguardhome --no-check-update --config /Users/${config.user.name}/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/AdguardHome.yaml"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/adguardhome-stderr.log";
      StandardOutPath = "/tmp/adguardhome-stdout.log";
    };
  };
}
