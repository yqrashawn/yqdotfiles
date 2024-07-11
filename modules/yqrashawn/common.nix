{ inputs, config, pkgs, ... }:
let
  # old = with pkgs.old; [ ];
  own = with pkgs.own;
    [ # lieer
    ];
  darwins = with pkgs.darwins; [ ];
  stables = with pkgs.stable; [
    difftastic
    notmuch
    streamlink
    you-get
    python3
    black
  ];
  masters = with pkgs.masters; [ adguardhome clojure-lsp clj-kondo ];
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
        emacs-lsp-booster
        (curl.override (args: { brotliSupport = true; }))
        # curl
        k6
        xcodes
        atuin
        # ntfy
        # postgresql
        # mpv
        mpv-unwrapped
        # rtx
        imagemagick
        zprint
        neil
        yt-dlp
        qbittorrent
        tailscale
        cloudflared
        todoist
        awscli2
        zellij
        kitty-themes
        # (docker.override (args: {
        #   buildxSupport = true;
        #   composeSupport = true;
        # }))
        # nyxt
        # docker-compose
        # docker-buildx
        # mutagen
        # mutagen-compose
        # colima
        docker-credential-helpers
        isync-isync
        # isync
        msmtp
        # opts at
        # https://github.com/NixOS/nixpkgs/blob/nixpkgs-unstable/pkgs/applications/editors/emacs/generic.nix
        (emacsWithPackagesFromUsePackage {
          package = (emacs30-overlay.override (args: {
            # withMacport = true;
            withNS = true;
            # macportVersion = "master";
            withSQLite3 = true;
            withWebP = true;
            withImageMagick = true;
            # withXwidgets = true;
            # nativeComp = true;
            withNativeCompilation = true;
            withTreeSitter = true;
            webkitgtk = true;
            # texinfo = true;
            # autoreconfHook = true;
            withCsrc = true;
          }));

          config = "./empty.el";
          defaultInitFile = false;
          extraEmacsPackages = epkgs: [ epkgs.jinx ];
        })

        ispell
        jet
        joker
        leiningen
        golangci-lint
        golangci-lint-langserver
        # obb
        # nbb

        # tools
        # notmuch
        vim
        cachix
        tmux
        lnav
        mu
        # macvim # use vim
        # bitwarden-cli # failed
        rbw
        # firefox
        # firefox-devedition-bin
        sqlite
        # yabai
        # sketchybar

        # cli tools
        noti
        ripgrep
        # ripgrep-all
        ugrep
        woff2
        zola
        git-interactive-rebase-tool
        btop
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
        # proxychains # build failed
        # proxychains-ng # no m1 support
        du-dust
        man
        duf # du
        yq
        fd
        ctags
        # gitui
        # httpie # failed to build
        # procs # ps
        bottom # btm htop
        thefuck
        nix-tree
        # nix-linter # build failed
        httpstat
        curlie
        act
        # glances # htop
        xh # httpie
        shfmt
        pgformatter
        sqlfluff
        aspell
        aspellDicts.en
        aspellDicts.en-computers
        aspellDicts.en-science
        hunspell
        hunspellDicts.en_US-large
        ccache
        dotenv-linter # failed
        reattach-to-user-namespace
        terminal-notifier
        enchant2
        w3m
        # starship
        neofetch
        broot # ranger
        # fasd
        # fzf
        coreutils-full
        findutils # xargs
        gnused
        pngquant
        optipng
        rbenv
        jless # json viewer
        less
        # zoxide # fasd
        lazygit
        git-branchless
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
        # bat
        bat-extras.batdiff
        bat-extras.batgrep
        bat-extras.batman
        bat-extras.batwatch
        bat-extras.prettybat
        gnutls
        openssl
        # pinentry
        pinentry-emacs
        # pinentry-qt
        # pinentry_mac
        # feroxbuster # content discovery
        ffmpeg
        pandoc
        # goku
        nix
        nixfmt
        nixpkgs-fmt
        nil
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
        # perl534Packages.GitAutofixup # https://github.com/torbiak/git-autofixup
        git-autofixup
        git-absorb # https://github.com/tummychow/git-absorb
        gnumake
        cmake
        dtach
        # python39Packages.percol # build failed
        # docker-compose_2
        docker-slim
        # urlview
        tmux-mem-cpu-load

        redis
        # langs
        tree-sitter
        nodePackages.js-beautify
        nodePackages.stylelint
        # nodePackages.expo-cli
        nodePackages.eslint
        nodePackages.typescript
        nodePackages.eslint_d
        nodePackages.markdownlint-cli2
        nodePackages.prettier
        nodePackages.nodemon
        nodePackages.serve
        nodePackages.surge
        nodePackages.neovim
        # nodePackages.web-ext
        nodePackages.textlint
        nodePackages.textlint-rule-write-good
        # php
        stylua
        # zls
        # gopls
        golint
        sqls
        gore
        gomodifytags
        gotests
        solc
        plantuml
        rustup
        # rust-analyzer
        (pkgs.ruby.withPackages (ps: with ps; [ rufo solargraph rubocop ]))

        # sbcl
        # asdf
        # lispPackages.asdf
        # lispPackages.quicklisp

        # ghc
        # guile
        # roswell
        clang-tools
        cmake-format
        html-tidy
        perl
        # openjdk

        watchman

        # lib
        zlib
        # libtool # https://github.com/WiseLibs/better-sqlite3/issues/243
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
      ] ++ masters ++ stables ++ darwins ++ own; # ++ old;
    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${pkgs.path}";
      stable.source = "${inputs.stable}";
    };
    # list of acceptable shells in /etc/shells
    shells = with pkgs; [ bash zsh "/bin/bash" "/bin/zsh" ];
    # systemPath = [ "/run/current-system/sw/bin" ];
  };

  fonts.packages = with pkgs; [ jetbrains-mono ];
}
