{ config, lib, pkgs, ... }: {
  imports = [ ./apps-minimal.nix ];
  homebrew = {
    casks = [
      "logseq"
      "google-chat"
      "voov-meeting"
      "cljstyle"
      "docker"
      "espanso"
      "xbar"
      "gfxcardstatus"
      "uhk-agent"
      "roam-research"
      # "kitty"
      "slack"
      "bitwarden"
      "clickup"
      "firefox"
      "firefox-developer-edition"
      "safari-technology-preview"
      "todoist"
      # "skim"
      "telegram"
      "discord"
      "dropbox"
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
