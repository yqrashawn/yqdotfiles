{ inputs, config, pkgs, ... }: {
  homebrew = {
    enable = true;
    onActivation = {
      upgrade = false;
      # cleanup = "uninstall";
      autoUpdate = false;
    };
    # https://daiderd.com/nix-darwin/manual/index.html#opt-homebrew.global
    global = {
      lockfiles = true;
      autoUpdate = false;
      brewfile = true;
    };

    taps = [
      "koekeishiya/formulae"
      "yqrashawn/goku"
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
      "borkdude/brew"
      "huahaiy/brew"
      "homebrew/cask-drivers"
      "incidentist/nyxt"
    ];
    # extraConfig = ''
    #   brew "yabai", restart_service: "changed"
    # '';
    brews = [
      # "yqrashawn/goku/goku"
      "alerter"
      "openssl@3" # conflux-rust
      "libgccjit"
      # "docker-completion"
      "curl"
      "php"
      "fcitx-remote-for-osx"
      # "luarocks"
      "zig"
      # "dum"
      # "clj-kondo"
      "editorconfig"
      "grip"
      # "fennel"
      "percol"
      # "proxychains-ng"
      "datalevin"
      "dotenv-linter"
      # "cloudflared"
      "janet"
      "pngpaste"

    ];
    casks = [
      "hammerspoon"
      "font-inter"
      "font-fira-mono-nerd-font"
      "font-freefont"
      "font-hack-nerd-font"
      "font-hasklug-nerd-font"
      "font-inconsolata-go-nerd-font"
      "font-inconsolata-lgc-nerd-font"
      "font-inconsolata-nerd-font"
      "font-iosevka-nerd-font"
      "font-jetbrains-mono-nerd-font"
      "font-liberation-nerd-font"
      "font-meslo-lg-nerd-font"
      "font-monofur-nerd-font"
      "font-monoid-nerd-font"
      "font-mononoki-nerd-font"
      "font-profont-nerd-font"
      "font-roboto-mono-nerd-font"
      "font-sauce-code-pro-nerd-font"
      "font-code-new-roman-nerd-font"
      "font-dejavu-sans-mono-nerd-font"
      "font-fira-code-nerd-font"
      "font-ubuntu-mono-nerd-font"
      "font-ubuntu-nerd-font"
    ];
  };
}
