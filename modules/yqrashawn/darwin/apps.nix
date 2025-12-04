{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./apps-minimal.nix ];
  homebrew = {
    casks = [
      "voov-meeting"
      "espanso"
      "xbar"
      "kitty"
      "languagetool-desktop"
      "telegram"
      "dropbox"
      "calibre"
      "adguard"
      "jordanbaird-ice"
      "zoom"
      "vivid-app"
      # "logseq"
      # "google-chat"
      # "cljstyle"
      # "docker"
      # "roam-research"
      # "slack"
      # "bitwarden"
      # "discord"
      # "gfxcardstatus"
      # "uhk-agent"
      # "clickup"
      # "firefox"
      # "firefox@developer-edition"
      # "safari-technology-preview"
      # "todoist"
      # "skim"
      # "adguard"
      # "stats"
      # "fleet"
      # "cursor"
      # "microsoft-edge@dev"
      # "macwhisper"
      # "gpg-suite-no-mail"
      # "syncthing"
      # "steam"
      # "android-studio"
      # "docker"
      # "flipper"
      # "microsoft-edge"
      # "playcover-community"
      # "spectacle"
      # "wechat"
    ];
    masApps = { };
  };
}
