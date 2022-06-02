{ config, lib, pkgs, ... }: {
  imports = [ ./apps-minimal.nix ];
  homebrew = {
    casks = [
      "xbar"
      "gfxcardstatus"
      "uhk-agent"
      "microsoft-edge-beta"
      "kitty"
      "slack"
      "bitwarden"
      "clickup"
      # "fork"
      "gpg-suite-no-mail"
      "firefox"
      "firefox-developer-edition"
      "todoist"
      "skim"
      "telegram"
      "discord"
      "dropbox"
      "calibre"
      "vivaldi"
      "ngrok"
      "adguard-nightly"
      # "syncthing"
      "stats"
      "zoom"
      "warp"
    ];
    masApps = { };
  };
}
