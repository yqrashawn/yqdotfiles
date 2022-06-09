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
      "railwaycat/emacsmacport"
      "koekeishiya/formulae"
      "teamookla/speedtest"
    ];
    # extraConfig = ''
    #   brew "yabai", restart_service: "changed"
    # '';
    brews = [
      "curl"
      "php"
      "fcitx-remote-for-osx"
      "luarocks"
      "zig"
      # "dum"
      "clj-kondo"
      "percol"
      "proxychains-ng"
      "dotenv-linter"
    ];
    casks = [ "hammerspoon" ];
  };
}
