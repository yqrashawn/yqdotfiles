{ config, lib, pkgs, ... }: {
  home.packages = [ pkgs.github-cli ];
  programs.git = {
    enable = true;
    aliases = {
      ignore =
        "!gi() { curl -sL https://www.toptal.com/developers/gitignore/api/$@ ;}; gi";
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
        ignorecase = false;
      };
      add = { interactive = { useBuiltin = false; }; };
      # http = {
      #   proxy = "http://127.0.0.1:6152";
      #   sslVerify = true;
      # };
      # https = {
      #   proxy = "http://127.0.0.1:6152";
      #   sslVerify = true;
      # };
      include = { path = "~/.gitconfig.local"; };
      alias = {
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
      filter = {
        lfs = {
          clean = "git-lfs clean -- %f";
          smudge = "git-lfs smudge -- %f";
          process = "git-lfs filter-process";
          required = true;
        };
      };
      merge = { conflictStyle = "zdiff3"; };
      branch = {
        autoSetupRebase = "always";
        autoSetupMerge = "always";
      };
      push = {
        default = "simple";
        followTags = true;
        autoSetupRemote = true;
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

      diff = {
        external =
          "${pkgs.difftastic}/bin/difft --color auto --background $(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo dark || echo light) --display side-by-side";
      };
      url."git@github.com:".insteadOf = "https://github.com/";
    };
    lfs.enable = true;
    # delta = { enable = true; };
    # custom in extraConfig.diff.external
    # difftastic = { enable = true; };
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
