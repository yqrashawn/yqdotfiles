{ config, lib, pkgs, ... }: {
  home.packages = [ pkgs.github-cli ];
  programs.git = {
    enable = true;
    userName = "holybasil";
    userEmail = "holybasil.1128@gmail.com";
    aliases = {
      prettylog = "...";
      fix = "commit --amend --no-edit";
      oops = "reset HEAD~1";
      sub = "submodule update --init --recursive";
    };
    extraConfig = {
      core = {
        editor = "emacsclient";
        excludesfile = "~/.gitignore_global";
        precomposeUnicode = true;
      };
      github = { user = "holybasil"; };
      http = {
        proxy = "http://127.0.0.1:6152";
        sslVerify = true;
      };
      https = { proxy = "http://127.0.0.1:6152"; };
      include = { path = "~/.gitconfig.local"; };
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
        submodule-updates = ''
          "!f(){ git submodule foreach 'git fetch origin master &> /dev/null; git --no-pager log --oneline HEAD..origin/master'; }; f"'';
        # ---- DWIM abort rebase, merge or cherry-pick
        abort = ''
          "!f() { local command=$(git status | grep -o "git \w* --abort"); echo $command; $($command); }; f"'';
      };
      pager = {
        pager = "delta";
        diff = "delta";
        reflog = "delta";
        show = "delta";
        difftool = true;
      };
      diff = { tool = "difftastic"; };
      difftool = {
        prompt = false;
        difftastic = { cmd = ''difft "$LOCAL" "$REMOTE"''; };
      };
      merge = { tool = "ediff"; };
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
        followTags = true;
      };
      pull = {
        ff = "only";
        rebase = true;
      };
      init = { defaultBranch = "main"; };

      credential.helper = if pkgs.stdenvNoCC.isDarwin then
        "osxkeychain"
      else
        "cache --timeout=1000000000";
      commit.verbose = true;
      fetch.prune = true;
    };
    lfs.enable = true;
    delta = {
      enable = true;
      options = {
        navigate = true;
        features = "side-by-side line-numbers decorations";
        syntax-theme = "Dracula";
        plus-style = ''syntax "#003800"'';
        minus-style = ''syntax "#3f0001"'';
        side-by-site = true;
        line-numbers = true;
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
      "#*#"
      "flycheck_*"
      "*.swp"
      ".clj-kondo/cache"

      "*.pyc"
    ];
  };
}
