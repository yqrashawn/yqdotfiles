{ config, lib, pkgs, ... }: {
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
      "firefox"
      "languagetool"
      "firefox-developer-edition"
      "safari-technology-preview"
      # "todoist"
      # "skim"
      "telegram"
      "discord"
      "dropbox"
      "calibre"
      "adguard-nightly"
      # "stats"
      "zoom"
      # "fleet"
      # "cursor"
      "microsoft-edge-beta"
      "macwhisper"
      # "gpg-suite-no-mail"
      # "steam"
      # "docker"
      "microsoft-edge"
      # "playcover-community"
      # "spectacle"
      # "wechat"
      "vivid"
    ];
    masApps = { };
  };
}
