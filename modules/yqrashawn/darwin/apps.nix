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
      # "logseq"
      # "google-chat"
      "voov-meeting"
      # "cljstyle"
      # "docker"
      "espanso"
      "xbar"
      # "gfxcardstatus"
      # "uhk-agent"
      "roam-research"
      "kitty"
      "slack"
      "bitwarden"
      # "clickup"
      # "firefox"
      "languagetool-desktop"
      # "firefox@developer-edition"
      # "safari-technology-preview"
      # "todoist"
      # "skim"
      "telegram"
      "discord"
      "dropbox"
      "calibre"
      "adguard"
      # "adguard"
      "stats"
      "zoom"
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
      "microsoft-edge"
      # "playcover-community"
      # "spectacle"
      # "wechat"
      "vivid-app"
    ];
    masApps = { };
  };
}
