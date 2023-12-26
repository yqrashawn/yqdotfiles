{ config, lib, pkgs, inputs, ... }:

let
  profileExtra = ''
    fpath=($HOME/.zfunc $fpath)
    autoload -Uz $fpath[1]/*
    ${lib.optionalString pkgs.stdenvNoCC.isLinux
    "[[ -e /etc/profile ]] && source /etc/profile"}
    [[ ! -f ~/Dropbox/sync/sync.zsh ]] || source ~/Dropbox/sync/sync.zsh
    . ${pkgs.asdf-vm}/etc/profile.d/asdf-prepare.sh
    . $HOME/.asdf/plugins/java/set-java-home.zsh

    if ! typeset -f _asdf > /dev/null; then
      fpath=(${pkgs.asdf-vm}/share/zsh/site-functions $fpath)
    fi
    # eval "$(${pkgs.rtx}/bin/rtx activate zsh)"
  '';
  bashProfileExtra = ''
    ${lib.optionalString pkgs.stdenvNoCC.isLinux
    "[[ -e /etc/profile ]] && source /etc/profile"}
    . ${pkgs.asdf-vm}/etc/profile.d/asdf-prepare.sh
    export JAVA_HOME=$(asdf where java)
    # eval "$(${pkgs.rtx}/bin/rtx activate zsh)"
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
    rreload = "rm -rf ~/.cache/prezto/zcompdump || true && exec $SHELL -l";
    cd = "z";
    j = "z";

    l = "eza -al";
    # ls = "eza -a";
    lsa = "eza -abghl --git --color=automatic";
    lsd = "eza -l --color=automatic | grep --color=never '^d'";
    lst = "eza --sort=created --time=created --long --all -r | sed 15q";
    y = "yarn";
    yi = "yarn init";
    ya = "yarn add";
    yad = "yarn add --dev";
    ysrc = "yarn start --reset-cache";
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
  };
in {
  imports = [ ./prezto.nix ];
  programs.bash.enable = true;
  programs.bash.shellAliases = aliases;
  programs.bash.initExtra = ''
    ${functions}
    # eval "$(${pkgs.rtx}/bin/rtx activate zsh)"
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
    # enableSyntaxHighlighting = true;
    enableAutosuggestions = true;
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
      eval "$(${pkgs.mcfly}/bin/mcfly init zsh)"
      # eval "$(${pkgs.rtx}/bin/rtx activate zsh)"

      dark-light-mcfly-history-widget() {
        unset MCFLY_LIGHT
        if [[ "$(defaults read -g AppleInterfaceStyle 2&>/dev/null)" != "Dark" ]]; then
            export MCFLY_LIGHT=TRUE
        fi
        mcfly-history-widget
      }

      zle -N dark-light-mcfly-history-widget
      bindkey '^R' dark-light-mcfly-history-widget

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
      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
    '';
    envExtra = ''
      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
    '';
    profileExtra = profileExtra;
    plugins = [
      {
        name = "zsh-fzf-tab";
        src = "${pkgs.zsh-fzf-tab}/share/fzf-tab";
        file = "fzf-tab.plugin.zsh";
      }
      {
        name = "zsh-autoquoter";
        src = inputs.zsh-autoquoter;
        file = "zsh-autoquoter.zsh";
      }
      {
        name = "zsh-autopair";
        src = inputs.zsh-autopair;
        file = "autopair.zsh";
      }
      {
        name = "fast-syntax-highlighting";
        file = "fast-syntax-highlighting.plugin.zsh";
        src = "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions";
      }
      # {
      #   # https://github.com/starship/starship/issues/1721#issuecomment-780250578
      #   # stop eating lines this is not pacman
      #   # rewrite C-r
      #   name = "zsh-vi-mode";
      #   file = "zsh-vi-mode.plugin.zsh";
      #   src = "${pkgs.zsh-vi-mode}/share/zsh-vi-mode/";
      # }
      {
        name = "alias-tips";
        src = inputs.zsh-alias-tips;
      }
      {
        name = "forgit";
        src = inputs.forgit;
        file = "forgit.plugin.zsh";
      }
      # {
      #   name = "zsh-abbrev-alias";
      #   src = inputs.zsh-abbrev-alias;
      #   file = "abbrev-alias.plugin.zsh";
      # }
    ];
    oh-my-zsh = {
      enable = false;
      plugins = [ "direnv" "aliases" "emacs" "yarn" "globalias" "take" ];
    };
  };
}
