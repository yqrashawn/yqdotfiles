{ config, lib, pkgs, ... }: {
  imports = [ ./apps-minimal.nix ];
  homebrew = {
    casks = [
      "cljstyle"
      "docker"
      "espanso"
      "xbar"
      "gfxcardstatus"
      "uhk-agent"
      # "microsoft-edge-beta"
      "kitty"
      "slack"
      "bitwarden"
      "clickup"
      "gpg-suite-no-mail"
      "firefox"
      "firefox-developer-edition"
      "todoist"
      "skim"
      "telegram"
      "discord"
      "dropbox-beta"
      "dropbox-passwords"
      "calibre"
      "ngrok"
      "adguard-nightly"
      # "syncthing"
      "stats"
      "zoom"
      # "warp"
    ];
    masApps = { };
  };
}
