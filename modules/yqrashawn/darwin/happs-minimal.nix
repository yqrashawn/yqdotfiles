{ config, lib, pkgs, ... }: {
  homebrew = {
    casks = [
      "iterm2"
      "alfred"
      # "bartender"
      # "spectacle"
      # "keybase"
      # "emacs-mac"
      "neteasemusic"
      "setapp"
      "notion"
      "hammerspoon"
      "rectangle-pro"
      # "alacritty"
      "figma"
      # "orion"
      # "miro"
      "iina"
      "lunar"
      # "monitorcontrol"
      "karabiner-elements"
      "google-chrome"
      "bettertouchtool"
      "input-source-pro"
      "raycast"
      "visual-studio-code"
      "nikitabobko/tap/aerospace"
    ];
  };
}
