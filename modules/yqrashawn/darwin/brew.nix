{
  inputs,
  config,
  pkgs,
  ...
}:
let
  homeDir = "/Users/${config.user.name}";

  # Homebrew 6 refuses to load formulae from untrusted third-party taps.
  # Derived from homebrew.taps below so the two can never disagree.
  trustStore = pkgs.writeText "homebrew-trust.json" (
    builtins.toJSON { trustedtaps = map (t: t.name) config.homebrew.taps; }
  );
in
{
  homebrew = {
    enable = true;
    onActivation = {
      upgrade = false;
      # cleanup = "uninstall";
      autoUpdate = false;
    };
    # https://daiderd.com/nix-darwin/manual/index.html#opt-homebrew.global
    global = {
      autoUpdate = false;
      brewfile = true;
    };

    taps = [
      "yqrashawn/goku"
      "secureauthcorp/tap"
      # "homebrew/cask-fonts"
      # "homebrew/cask-drivers"
      # "homebrew/cask-versions"
      "railwaycat/emacsmacport"
      "teamookla/speedtest"
      "borkdude/brew"
      "huahaiy/brew"
      "incidentist/nyxt"
      "nikitabobko/tap"
      "buildkite/buildkite"
      "candid82/brew"
      "smokris/getwindowid"
      "stripe/stripe-cli"
      "jimeh/emacs-builds"
    ];
    # extraConfig = ''
    #   brew "yabai", restart_service: "changed"
    # '';
    brews = [
      # "alerter"
      "yqrashawn/goku/goku"
      "secureauthcorp/tap/oauth2c"
      "flyctl"
      "percol"
      "huahaiy/brew/datalevin"
      "candid82/brew/joker"
      "buildkite/buildkite/buildkite-agent"
      "smokris/getwindowid/getwindowid"
      "stripe/stripe-cli/stripe"
      "fcitx-remote-for-osx"
    ];
    casks = [
      "hammerspoon"
      "jimeh/emacs-builds/emacs-app"
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

  # Homebrew reads its trust store from $XDG_CONFIG_HOME/homebrew/trust.json
  # when that variable is set and ~/.homebrew/trust.json otherwise. The homebrew
  # activation step runs `sudo --preserve-env=PATH`, which strips
  # XDG_CONFIG_HOME, so both paths must resolve to one file — otherwise trust
  # granted from an interactive shell is invisible to `brew bundle`.
  system.activationScripts.preActivation.text = ''
    echo "seeding Homebrew trust store..." >&2
    install -d -o ${config.user.name} -g staff -m 700 ${homeDir}/.config/homebrew
    install -o ${config.user.name} -g staff -m 600 ${trustStore} ${homeDir}/.config/homebrew/trust.json

    if [ -L ${homeDir}/.homebrew ] || [ ! -e ${homeDir}/.homebrew ]; then
      ln -sfn ${homeDir}/.config/homebrew ${homeDir}/.homebrew
      chown -h ${config.user.name}:staff ${homeDir}/.homebrew
    else
      echo "warning: ${homeDir}/.homebrew is not a symlink; Homebrew trust may not resolve" >&2
    fi
  '';
}
