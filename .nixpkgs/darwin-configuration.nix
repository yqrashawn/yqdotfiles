{ config, pkgs, ... }:

let
  # sources = import ./nix/sources.nix;
  # niv = import sources.niv { inherit pkgs; };
  # comma = import sources.comma {};
  # all-hies = import sources.all-hies {};
  # hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
  username = builtins.getEnv "USER";
  homeDir = "/Users/${username}";
  # nix-direnv = import sources.nix-direnv {};
in
{
  imports = [ <home-manager/nix-darwin> ];


  home-manager.useUserPackages = true;

  users.users.${username} = {
    home = homeDir;
    description = "${username}'s account'";
    shell = pkgs.zsh;
  };

  home-manager.users.${username} = import ../.config/nixpkgs/home.nix {
    inherit config;
    inherit pkgs;
    # inherit lib;
    inherit username;
    inherit homeDir;
    # inherit nix-direnv;
  };

  # install docs for systemPackages
  documentation = {
    enable = true;
    doc = {
      enable = true;
    };
    info = {
      enable = true;
    };
    man = {
      enable = true;
    };
  };


  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
       (writeShellScriptBin "nixFlakes" ''
      exec ${nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
    '')

     # clojure
    clojure
    clojure-lsp
    ispell
    isync
    babashka
    clj-kondo
    joker
    leiningen
    obb
    # nbb

    # tools
    notmuch
    tmux
    zsh
    bash
    git
    neovim
    mu
    # macvim # use vim
    bitwarden-cli
    # firefox
    # firefox-devedition-bin
    sqlite
    yabai
    sketchybar

    # cli tools
    ripgrep
    htop
    hub
    mcfly
    fnm
    shellcheck
    proselint
    trash-cli
    delta
    duf # du
    jq
    fd
    # httpie # failed to build
    tldr
    procs # ps
    bottom # btm htop
    thefuck
    curlie # httpie
    glances # htop
    xh # httpie
    shfmt
    hunspell
    enchant # ispell
    w3m
    starship
    broot # ranger
    direnv
    exa
    fasd
    fzf
    coreutils-full
    pngquant
    rbenv
    jless # json viewer
    gnupg
    zoxide # fasd
    lazygit
    watchexec
    wget
    curl
    # grip # markdown preview, failed to build
    multimarkdown
    brotli
    automake
    autoconf
    bat
    gnutls
    openssl
    ffmpeg
    pandoc
    cmake
    goku

    # langs
    yarn
    lua
    deno
    fennel
    lua
    luarocks
    # zig # marked broken
    go
    plantuml
    rustup

    # entertainment
    youtube-dl
    yt-dlp
    streamlink
    mpv
    you-get

    # lib
    # libgccjit
    rlwrap
    readline
    llvm
    # texinfo # cllision to pod2texi
    pkg-config
    pcre

    # not available
    # du
    # percol
    ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  nix.package = pkgs.nixUnstable;
  services.nix-daemon.enable = true;

  # program.nix-index.enable = true;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  fonts.enableFontDir = true;

  environment.variables = {
    EDITOR = "emacsclient";
  };
}
