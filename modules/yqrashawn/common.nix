{
  inputs,
  config,
  pkgs,
  emacs,
  ...
}:
let
  own = with pkgs.own; [ ];
  darwins = with pkgs.darwins; [
    difftastic
    streamlink
    you-get
    black
    mpv-unwrapped
    groff
    tailscale
  ];
  stables = with pkgs.stable; [ ];
  masters = with pkgs.masters; [
    gitleaks
    trufflehog
    clj-kondo
    # mise
    zprint
    neil
    bbin
    jet
    zsh-fzf-tab
    zsh-forgit
    zsh-f-sy-h
    zsh-autopair
    # flyctl
  ];
  emacs = pkgs.callPackage ./emacs {
    emacs = pkgs.emacs30-overlay;
    inherit inputs;
  };
in
{
  imports = [
    ./primary.nix
    ./nixpkgs.nix
    ./overlays.nix
    ./etc-zsh.nix
  ];

  # user -> users.users.<primary user>
  user = {
    description = "Rashawn Zhang";
    home = "${if pkgs.stdenvNoCC.isDarwin then "/Users" else "/home"}/${config.user.name}";
    shell = pkgs.zsh;
  };

  # bootstrap home manager using system config
  # hm -> home-manager.users.<primary user>
  hm = import ./home-manager/default.nix;

  # let nix manage home-manager profiles and use global nixpkgs
  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
  };

  # environment setup
  environment = {
    systemPackages =
      with pkgs;
      [
        eask-cli
        # firefox-devedition
        lzip
        clojure-lsp
        notmuch
        # zed-editor
        emacs-lsp-booster
        (curl.override (args: {
          brotliSupport = true;
        }))
        # curl
        k6
        xcodes
        jsonnet
        inputs.atuin.packages.${pkgs.system}.default
        # ntfy
        postgresql
        # rtx
        imagemagick
        yt-dlp
        qbittorrent
        cloudflared
        # awscli2
        # zellij
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
        # isync-isync
        (isync.override { withCyrusSaslXoauth2 = true; })
        msmtp
        mkcert
        emacs
        ispell
        joker
        leiningen
        golangci-lint
        golangci-lint-langserver
        # obb
        # nbb

        # tools
        vim
        cachix
        tmux
        lnav
        mu
        # macvim # use vim
        # bitwarden-cli # failed
        rbw
        sqlite
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
        dust
        man
        duf # du
        yq
        fd
        q
        ctags
        # gitui
        # httpie # failed to build
        # procs # ps
        bottom # btm htop
        nix-tree
        # nix-linter # build failed
        httpstat
        curlie
        # act
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
        # git-branchless
        # watchexec
        wget
        # curl
        rsync
        # treefmt
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
        nixd
        nixfmt
        nixpkgs-fmt
        nil # lang server for nix
        openssh
        neofetch
        parallel
        # terraform
        # vagrant
        pre-commit
        # espanso # build failed
        nix-doc
        gawk
        sd
        asdf-vm
        vmtouch
        # commitizen
        graphviz
        # perl534Packages.GitAutofixup # https://github.com/torbiak/git-autofixup
        git-extras
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
        rustywind
        nodePackages.js-beautify
        nodePackages.stylelint
        # nodePackages.expo-cli
        # nodePackages.eslint
        nodePackages.typescript
        nodePackages.eslint_d
        nodePackages.markdownlint-cli2
        nodePackages.prettier
        nodePackages.nodemon
        nodePackages.serve
        nodePackages.surge
        # nodePackages.neovim
        # nodePackages.web-ext
        nodePackages.textlint
        nodePackages.textlint-rule-write-good
        # php
        stylua
        # zls
        # gopls
        # golint
        sqls
        # gore # go repl
        # gomodifytags
        # gotests
        solc
        plantuml
        rustup
        # rust-analyzer
        (pkgs.ruby.withPackages (
          ps: with ps; [
            rufo
            solargraph
            rubocop
          ]
        ))

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
      ]
      ++ masters
      ++ stables
      ++ darwins
      ++ own;
    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${pkgs.path}";
      stable.source = "${inputs.stable}";
    };
    # list of acceptable shells in /etc/shells
    shells = with pkgs; [
      bash
      zsh
      "/bin/bash"
      "/bin/zsh"
    ];
    # systemPath = [ "/run/current-system/sw/bin" ];
  };

  fonts.packages = with pkgs; [
    jetbrains-mono
    nerd-fonts.jetbrains-mono
    nerd-fonts.hack
    nerd-fonts.fira-mono
    # nerd-fonts.inconsolata
    # nerd-fonts.iosevka
  ];
}
