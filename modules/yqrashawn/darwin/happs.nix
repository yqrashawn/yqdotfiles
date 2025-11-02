{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./happs-minimal.nix ];
  homebrew = {
    casks = [
      "voov-meeting"
      # "espanso"
      # "xbar"
      # "roam-research"
      # "kitty"
      "slack"
      "bitwarden"
      "clickup"
      "languagetool-desktop"
      # "firefox@developer-edition"
      # "safari-technology-preview"
      # "todoist"
      # "skim"
      "telegram"
      "discord"
      "dropbox"
      "calibre"
      # "adguard@nightly"
      "adguard"
      # "stats"
      "zoom"
      # "fleet"
      # "cursor"
      # "microsoft-edge@dev"
      # "macwhisper"
      # "gpg-suite-no-mail"
      # "steam"
      # "docker"
      "microsoft-edge"
      # "playcover-community"
      # "spectacle"
      # "wechat"
      "vivid-app"
    ];
    masApps = { };
  };
}
