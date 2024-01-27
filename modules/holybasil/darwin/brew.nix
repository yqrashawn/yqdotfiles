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
      "homebrew/cask-fonts"
      "homebrew/cask-drivers"
      "homebrew/cask-versions"
      "homebrew/services"
      "homebrew/command-not-found"
      "koekeishiya/formulae"
      "teamookla/speedtest"
      "homebrew/cask-drivers"
    ];
    # extraConfig = ''
    #   brew "yabai", restart_service: "changed"
    # '';
    brews = [ "curl" "php" "luarocks" ];
    casks = [ "hammerspoon" "wacom-tablet" ];
  };
}
