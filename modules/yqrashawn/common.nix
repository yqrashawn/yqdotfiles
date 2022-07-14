{ inputs, config, pkgs, ... }:
let
  # old = with pkgs.old; [ ];
  own = with pkgs.own; [ lieer isync-isync cyrus_sasl_xoauth2 ];
  darwins = with pkgs.darwins; [ ];
  stables = with pkgs.stable; [ notmuch yt-dlp streamlink mpv you-get python3 ];
in {
  imports = [ ./primary.nix ./nixpkgs.nix ./overlays.nix ./etc-zsh.nix ];

  user = {
    description = "Rashawn Zhang";
    home = "${
        if pkgs.stdenvNoCC.isDarwin then "/Users" else "/home"
      }/${config.user.name}";
    shell = pkgs.zsh;
  };

  # bootstrap home manager using system config
  hm = import ./home-manager;

  # let nix manage home-manager profiles and use global nixpkgs
  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
    # backupFileExtension = "backup";
  };

  # environment setup
  environment = {
    systemPackages = with pkgs;
      [
        # emacsMacport
        # clojure
        clojure-lsp
        ispell
        # isync
        babashka
        joker
        leiningen
        obb
        # nbb

        # tools
        # notmuch
        vim
        cachix
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
        woff2
        cmus
        zola
        git-interactive-rebase-tool
        htop
        hub
        gh
        # fnm
        doxygen
        gmailctl
        shellcheck
        proselint
        trash-cli
        delta
        entr
        # proxychains-ng # build failed
        du-dust
        duf # du
        jq
        fd
        ctags
        # gitui
        # httpie # failed to build
        tealdeer # tldr in rust
        # procs # ps
        bottom # btm htop
        thefuck
        nix-tree
        nix-linter
        curlie # httpie
        # glances # htop
        xh # httpie
        shfmt
        hunspell
        ccache
        # dotenv-linter # failed
        reattach-to-user-namespace
        terminal-notifier
        enchant # ispell
        w3m
        starship
        broot # ranger
        direnv
        exa
        # fasd
        fzf
        skim
        coreutils-full
        gnused
        pngquant
        optipng
        rbenv
        jless # json viewer
        gnupg
        less
        zoxide # fasd
        lazygit
        # watchexec
        wget
        # curl
        rsync
        treefmt
        # grip # markdown preview, failed to build
        multimarkdown
        languagetool
        mujs
        brotli
        automake
        autoconf
        bat
        bat-extras.batdiff
        bat-extras.batgrep
        bat-extras.batman
        bat-extras.batwatch
        bat-extras.prettybat
        gnutls
        openssl
        pinentry
        pinentry_mac
        feroxbuster
        ffmpeg
        pandoc
        cmake
        # goku
        nix
        nixfmt
        nixpkgs-fmt
        rnix-lsp
        openssh
        neofetch
        parallel
        # terraform
        # vagrant
        pre-commit
        # espanso # build failed
        nix-doc
        gawk
        flyctl
        jet
        sd
        asdf-vm
        vmtouch
        # commitizen
        graphviz
        perl534Packages.GitAutofixup # https://github.com/torbiak/git-autofixup
        git-absorb # https://github.com/tummychow/git-absorb
        gnumake
        cmake
        dtach
        difftastic
        # python39Packages.percol # build failed
        # docker-compose_2

        redis
        # langs
        tree-sitter
        nodePackages.expo-cli
        nodePackages.eslint
        nodePackages.diagnostic-languageserver
        nodePackages.typescript
        nodePackages.eslint_d
        nodePackages.markdownlint-cli2
        nodePackages.prettier
        nodePackages.nodemon
        nodePackages.serve
        nodePackages.surge
        nodePackages.neovim
        nodePackages.live-server
        nodePackages.web-ext
        nodePackages.textlint
        nodePackages.textlint-rule-write-good
        nodePackages.typescript-language-server
        nodePackages.vscode-css-languageserver-bin
        nodePackages.vscode-html-languageserver-bin
        nodePackages.vscode-css-languageserver-bin
        nodePackages.vscode-langservers-extracted
        nodePackages.vscode-json-languageserver
        nodePackages.dockerfile-language-server-nodejs
        nodePackages.bash-language-server
        nodePackages.yaml-language-server
        yarn
        # php
        lua
        # luajit
        stylua
        # deno
        # fennel
        # janet # broken
        # luarocks
        # zig # marked broken
        go
        # golangci-lint
        plantuml
        rustup
        rust-analyzer
        (pkgs.ruby.withPackages (ps: with ps; [ rufo solargraph ]))

        # ghc
        # chez
        sbcl
        # guile
        roswell
        clang-tools
        html-tidy
        ninja
        perl
        # openjdk

        watchman

        # lib
        # libgccjit
        zlib
        libtool
        cyrus_sasl # isync xoauth2
        rlwrap
        readline
        llvm
        glib # libvterm
        # texinfo # cllision to pod2texi
        pkg-config
        # pcre
        # pcre2
        harfbuzz
        librsvg
        imagemagick
        # openjpeg

        # not available
        # du
      ] ++ stables ++ darwins ++ own; # ++ old;
    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${pkgs.path}";
      stable.source = "${inputs.stable}";
    };
    # list of acceptable shells in /etc/shells
    shells = with pkgs; [ bash zsh "/bin/bash" "/bin/zsh" ];
    systemPath = [ "/run/current-system/sw/bin" ];
  };

  fonts.fonts = with pkgs; [ jetbrains-mono ];
}
