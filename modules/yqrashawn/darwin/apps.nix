{ config, lib, pkgs, ... }: {
  imports = [ ./apps-minimal.nix ];
  homebrew = {
    casks = [
      "logseq"
      "google-chat"
      "voov-meeting"
      "cljstyle"
      # "docker"
      "espanso"
      "xbar"
      "gfxcardstatus"
      "uhk-agent"
      "kitty"
      "slack"
      "bitwarden"
      "clickup"
      "firefox"
      "firefox-developer-edition"
      "todoist"
      "skim"
      "telegram"
      "discord"
      "dropbox"
      "dropbox-passwords"
      "calibre"
      "adguard-nightly"
      "stats"
      "zoom"
      # "microsoft-edge-beta"
      # "gpg-suite-no-mail"
      # "ngrok"
      # "syncthing"
    ];
    masApps = { };
  };
}
