{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "yqrashawn";
  home.homeDirectory = "/Users/yqrashawn";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
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
    macvim
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
    texinfo
    pkg-config
    pcre

    # not available
    # du
    # percol
  ];

  programs.emacs = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "yqrashawn";
    userEmail = "namy.19@gmail.com";
    aliases = {
      prettylog = "...";
    };
    extraConfig = {
      core = {
        editor = "emacsclient";
        excludesfile = "~/.gitignore_global";
        precomposeUnicode = true;
      };
      github = {
        user = "yqrashawn";
      };
      http = {
        proxy = "http://127.0.0.1:6152";
      };
      https = {
        proxy = "http://127.0.0.1:6152";
      };
      include = {
        path = "~/.gitconfig.local";
      };
      alias = {

        dft = "difftool";
        fetch = "git fetch --tags";
        branches = "!legit branches";
        publish = "!legit publish";
        unpublish = "!legit unpublish";
        sync = "!legit sync";
        switch = "!legit switch";
        resync = "!legit resync";
        undo = "!legit undo";
        # ---- git-submodule-update-checker
        submodule-updates = "\"!f(){ git submodule foreach 'git fetch origin master &> /dev/null; git --no-pager log --oneline HEAD..origin/master'; }; f\"";
        # ---- DWIM abort rebase, merge or cherry-pick
        abort = "\"!f() { local command=$(git status | grep -o \"git \\w* --abort\"); echo $command; $($command); }; f\"";
      };
      pager = {
        pager = "delta";
        diff = "delta";
        reflog = "delta";
        show = "delta";
        difftool = true;
      };
      diff = {
        tool = "difftastic";
      };
      difftool = {
        prompt = false;
        difftastic = {
          cmd = "difft \"$LOCAL\" \"$REMOTE\"";
        };
      };
      merge = {
        tool = "ediff";
      };
      mergetool = {
        ediff = {
          cmd = "~/local/bin/ediff-merge-script $LOCAL $REMOTE $MERGED $BASE";
          trustExitCode = true;
        };
      };
      filter = {
        lfs = {
          clean = "git-lfs clean -- %f";
          smudge = "git-lfs smudge -- %f";
          process = "git-lfs filter-process";
          required = true;
        };
      };
      interactive = {
        diffFilter = "delta --color-only";
      };
      delta = {
        navigate=true;
        features = "side-by-side line-numbers decorations";
        syntax-theme = "Dracula";
        plus-style = "syntax \"#003800\"";
        minus-style = "syntax \"#3f0001\"";
        decorations = {
          commit-decoration-style = "bold yellow box ul";
          file-style = "bold yellow ul";
          file-decoration-style = "none";
          hunk-header-decoration-style = "cyan box ul";
        };
        line-numbers = {
          line-numbers-left-style = "cyan";
          line-numbers-right-style = "cyan";
          line-numbers-minus-style = 124;
          line-numbers-plus-style = 28;
        };
      };
      color = {
        ui = true;
        diff = {
          # meta = 277;
          frag = "magenta bold";
          commit = "227 bold";
          old = "red bold";
          new = "green bold";
          whitespacw = "red reverse";
        };
        diff-highlight = {
          oldNormal = "red bold";
          oldHighlight = "red bold 52";
          newNormal = "green bold";
          newHighlight = "green bold 22";
        };
      };
      push = {
        default = "simple";
      };
      pull = {
        ff = "only";
      };
      init = {
        defaultBranch = "main";
      };
    };
    ignores = [
      # Compiled source #
      ###################
      "*.com"
      "*.class"
      "*.dll"
      "*.exe"
      "*.o"
      "*.so"
      # Packages #
      ############
      # it's better to unpack these files and commit the raw source
      # git has its own built in compression methods
      "*.iso"


      # Logs and databases #
      ######################
      "*.log"
      "*.sqlite"

      # OS generated files #
      ######################
      ".DS_Store"
      ".DS_Store?"
      "._*"
      ".Spotlight-V100"
      ".Trashes"
      "hthumbs.db"
      "humbs.db"

      ".tern-port"
      ".indium"
      ".tabnine_root"
      ".ignore"

      # worklog
      "*.worklog.org"
      "*~"
      "\#*#"
      "flycheck_*"
      "*.swp"
      ".clj-kondo/cache"

      "*.pyc"
    ];
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "GitHub";
      italic-text = "always";
    };
  };
}
