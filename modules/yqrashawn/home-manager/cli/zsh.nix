{ config, lib, pkgs, inputs, ... }:

let
  envExtra = ''
    . ${pkgs.asdf-vm}/etc/profile.d/asdf-prepare.sh
    . $HOME/.asdf/plugins/java/set-java-home.zsh

    if ! typeset -f _asdf > /dev/null; then
      fpath=(${pkgs.asdf-vm}/share/zsh/site-functions $fpath)
    fi
    export PNPM_HOME="/$HOME/.local/share/pnpm"
  '';
  zshProfileExtra = ''
    fpath=($HOME/.zfunc $fpath)
    autoload -Uz $fpath[1]/*
    ${lib.optionalString pkgs.stdenvNoCC.isLinux
    "[[ -e /etc/profile ]] && source /etc/profile"}
    [[ ! -f ~/Dropbox/sync/sync.zsh ]] || source ~/Dropbox/sync/sync.zsh
    # eval "$(${pkgs.masters.mise}/bin/mise activate zsh)"
  '';
  bashProfileExtra = ''
    ${lib.optionalString pkgs.stdenvNoCC.isLinux
    "[[ -e /etc/profile ]] && source /etc/profile"}
    . ${pkgs.asdf-vm}/etc/profile.d/asdf-prepare.sh
    export JAVA_HOME=$(asdf where java)
    export PNPM_HOME="$HOME/.local/share/pnpm"
    # eval "$(${pkgs.masters.mise}/bin/mise activate bash)"
  '';
  functions = builtins.readFile ./functions.sh;
  aliases = lib.mkIf pkgs.stdenvNoCC.isDarwin {
    # darwin specific aliases
    ibrew = "arch -x86_64 brew";
    abrew = "arch -arm64 brew";
    caski = "brew install --cask";
    caskr = "brew reinstall --cask";
    casko = "brew outdated --cask";
    casku = "brew upgrade --cask";
    rm = "trash";
    d = "cd ~/Downloads/";
    wo = "cd ~/workspace/office";
    wt = "cd ~/workspace/third";
    wh = "cd ~/workspace/home";
    o = "open";
    p2 = "percol";
    cleanup = "find . -type f -name '*.DS_Store' -ls -delete";
    reload = "exec $SHELL -l";
    rreload = "trash ~/.cache/prezto/zcompdump || true && exec $SHELL -l";
    cd = "z";
    j = "z";

    l = "eza -al";
    # ls = "eza -a";
    lsa = "eza -abghl --git --color=automatic";
    lsd = "eza -l --color=automatic | grep --color=never '^d'";
    lst = "eza --sort=created --time=created --long --all -r | sed 15q";
    p = "pnpm";
    pi = "pnpm i";
    pga = "pnpm i -g";
    px = "pnpx";
    y = "yarn";
    yi = "yarn init";
    ya = "yarn add";
    yad = "yarn add --dev";
    ystnc = "yarn start --reset-cache";
    yga = "yarn global add";
    yu = "yarn upgrade";
    yui = "yarn upgrade-interactive";
    yv = "yarn version";
    yvc = "yarn version check";
    yvi = "yarn version check --interactive";
    yst = "yarn start";
    yd = "yarn dev";
    yx = "yarn remove";
    yw = "yarn workspace";
    mpva = "mpv --no-video";
    mpvv = "mpv --no-audio";
    # curl = "curlie";
    # curl = "httpie";

    gfc1 = "git clone --depth 1";

    brewh = "brew home";
    brewss = "brew services start";
    brewsr = "brew services restart";
    brewsx = "brew services stop";
    e = "~/local/bin/em";
    # man = "nocorrect /etc/profiles/per-user/${config.user.name}/bin/man";
    # man = "nocorrect ${pkgs.man}/bin/man";
  };
in {
  imports = [ ./prezto.nix ];
  programs.bash.enable = true;
  programs.bash.shellAliases = aliases;
  programs.bash.initExtra = ''
    ${functions}
  '';
  programs.bash.profileExtra = bashProfileExtra;
  programs.zsh = let
    mkZshPlugin = { pkg, file ? "${pkg.pname}.plugin.zsh" }: rec {
      name = pkg.pname;
      src = pkg.src;
      inherit file;
    };
  in {
    enable = true;
    autocd = true;
    enableCompletion = true;
    autosuggestion = { enable = true; };
    # enableSyntaxHighlighting = true;
    shellGlobalAliases = {
      UUID = "$(uuidgen | tr -d \\n)";
      G = "| grep";
    };
    dirHashes = {
      home = "$HOME";
      docs = "$HOME/Documents";
      vids = "$HOME/Videos";
      dl = "$HOME/Downloads";
    };
    localVariables = {
      LANG = "en_US.UTF-8";
      GPG_TTY = "/dev/ttys000";
      DEFAULT_USER = "${config.home.username}";
      CLICOLOR = 1;
      EDITOR = "emacsclient";
      VISUAL = "emacsclient";
      LS_COLORS = "ExFxBxDxCxegedabagacad";
      TERM = "xterm-256color";
    };
    shellAliases = aliases;
    initExtraBeforeCompInit = ''
      fpath+=/nix/var/nix/profiles/system/sw/share/zsh/site-functions
      fpath+=/nix/var/nix/profiles/system/sw/share/zsh/$ZSH_VERSION/functions
      fpath+=/nix/var/nix/profiles/system/sw/share/zsh/vendor-completions
    '';
    loginExtra = ''
      # https://github.com/alacritty/alacritty/issues/2950
      # disable alacritty icon bouncing
      printf "\e[?1042l"
    '';
    initExtra = ''
      eval "$(${pkgs.atuin}/bin/atuin init zsh)"

      ### TRAMP
      # Stop TRAMP (in Emacs) from hanging or term/shell from echoing back commands
      #if [[ $TERM == dumb || -n $INSIDE_EMACS ]]; then
      #  unsetopt zle prompt_cr prompt_subst
      #  whence -w precmd >/dev/null && unfunction precmd
      #  whence -w preexec >/dev/null && unfunction preexec
      #  PS1='$ '
      #fi

      ${functions}
      ${lib.optionalString pkgs.stdenvNoCC.isDarwin ''
        # if [[ -d /opt/homebrew ]]; then
        #   eval "$(/opt/homebrew/bin/brew shellenv)"
        # fi
      ''}
      unset RPS1
      [[ ! -f ~/.local.zsh ]] || source ~/.local.zsh
    '';
    envExtra = envExtra;
    profileExtra = zshProfileExtra;
    plugins = [
      # {
      #   name = "zsh-fzf-tab";
      #   src = "${pkgs.zsh-fzf-tab}/share/fzf-tab";
      #   file = "fzf-tab.plugin.zsh";
      # }
      {
        name = "zsh-autoquoter";
        src = inputs.zsh-autoquoter;
        file = "zsh-autoquoter.zsh";
      }
      {
        name = "fast-syntax-highlighting";
        file = "fast-syntax-highlighting.plugin.zsh";
        src = "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions";
      }
      {
        name = "alias-tips";
        src = inputs.zsh-alias-tips;
      }
    ];
    oh-my-zsh = {
      enable = false;
      plugins = [ "direnv" "aliases" "emacs" "yarn" "globalias" "take" ];
    };
  };
}
