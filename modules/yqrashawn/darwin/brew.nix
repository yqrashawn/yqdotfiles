{ inputs, config, pkgs, ... }: {
  homebrew = {
    enable = true;
    autoUpdate = false;
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
      "railwaycat/emacsmacport"
      "koekeishiya/formulae"
      "teamookla/speedtest"
    ];
    # extraConfig = ''
    #   brew "yabai", restart_service: "changed"
    # '';
    casks = [ "hammerspoon" ];
  };
}
