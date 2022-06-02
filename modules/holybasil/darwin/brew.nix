{ inputs, config, pkgs, ... }: {
  homebrew = {
    enable = true;
    autoUpdate = true;
    global = {
      brewfile = true;
      noLock = true;
    };

    taps = [
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-drivers"
      "homebrew/cask-versions"
      "homebrew/core"
      "homebrew/services"
      "homebrew/command-not-found"
      "koekeishiya/formulae"
      "teamookla/speedtest"
    ];
    # extraConfig = ''
    #   brew "yabai", restart_service: "changed"
    # '';
    casks = [ "hammerspoon" ];
  };
}
