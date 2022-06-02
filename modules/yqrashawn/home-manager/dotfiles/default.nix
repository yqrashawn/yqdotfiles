{ inputs, config, pkgs, lib, ... }: {
  home.file = {
    hammerspoon = {
      source = inputs.spacehammer;
      target = ".hammerspoon";
      recursive = true;
    };
    # Spoons = {
    #   source = ./Spoons;
    #   target = ".hammerspoon/Spoons";
    #   recursive = true;
    # };
    spacehammer = {
      source = ./.spacehammer;
      target = ".spacehammer";
      recursive = true;
    };
    asdf = {
      source = ./.asdfrc;
      target = ".asdfrc";
    };
    ripgrep = {
      source = ./.ripgreprc;
      target = ".ripgreprc";
    };
    editorconfig = {
      source = ./.editorconfig;
      target = ".editorconfig";
    };
    local-bins = {
      source = ./local-bins;
      target = "local/bin";
      recursive = true;
    };
    raycast = lib.mkIf pkgs.stdenvNoCC.isDarwin {
      source = ./raycast;
      target = ".local/bin/raycast";
      recursive = true;
    };
    zfunc = {
      source = ./zfunc;
      target = ".zfunc";
      recursive = true;
    };
    percol = {
      source = ./.percol.d;
      target = ".percol.d";
      recursive = true;
    };
    datomic = {
      source = ./.datomic;
      target = ".datomic";
      recursive = true;
    };
    prettier = {
      source = ./.prettierrc;
      target = ".perttierrc";
    };
    tern = {
      source = ./.tern-config;
      target = ".tern-config";
    };
    textlint = {
      source = ./.textlintrc;
      target = ".textlintrc";
    };
    tridactyl = {
      source = ./.tridactylrc;
      target = ".tridactylrc";
    };
    nvm = {
      source = ./.nvmrc;
      target = ".nvmrc";
    };
    notmuch-config = {
      source = ./.notmuch-config;
      target = ".notmuch-config";
    };

    gmailieer1 = {
      source = ./gmailieer.main.json;
      target = ".mail/account.gmail/.gmailieer.json";
    };
    gmailieer2 = {
      source = ./gmailieer.2.json;
      target = ".mail/account.yqrashawn/.gmailieer.json";
    };
    shadow-cljs = {
      source = ./shadow-cljs.edn;
      target = ".shadow-cljs/config.edn";
    };
    lein = {
      source = ./.lein;
      target = ".lein";
      recursive = true;
    };
    ssh = {
      source = ./ssh.gpg;
      target = ".ssh/config.gpg";
    };
    osascript = {
      source = ./osascript;
      target = "osascript";
      recursive = true;
    };
    authinfo = {
      source = ./.authinfo.gpg;
      target = ".authinfo.gpg";
    };
    tabnine = {
      source = ./TabNine.toml;
      target = "Library/Preferences/TabNine/TabNine.toml";
    };
    gitignoreglobal = {
      source = ./gitignore_global;
      target = ".gitignore_global";
    };
    husky = {
      source = ./.huskyrc;
      target = ".huskyrc";
    };
    mbsync = {
      source = ./.mbsyncrc;
      target = ".mbsyncrc";
    };
    mailcap = {
      source = ./.mailcap;
      target = ".mailcap";
    };
    mdl = {
      source = ./.mdlrc;
      target = ".mdlrc";
    };
    sbclrc = {
      source = ./.sbclrc;
      target = ".sbclrc";
    };
    phoenix = {
      source = ./.phoenix.js;
      target = ".phoenix.js";
    };
    # npmrc = {
    #   text = ''
    #     prefix = ${config.home.sessionVariables.NODE_PATH};
    #   '';
    #   target = ".npmrc";
    # };
  };

  xdg.enable = true;
  xdg.configFile = {
    "nixpkgs/config.nix".source = ../../config.nix;
    yabai = lib.mkIf pkgs.stdenvNoCC.isDarwin {
      source = ./yabai;
      recursive = true;
    };
    karabiner = {
      source = ./karabiner.edn;
      target = "karabiner.edn";
    };
    topgrade = {
      source = ./topgrade.toml;
      target = "topgrade.toml";
    };
    bblib = {
      source = ./bblib;
      recursive = true;
    };
    sketchybar = {
      source = ./sketchybar;
      recursive = true;
    };
    "enchant" = {
      source = ./enchant;
      recursive = true;
    };
    "clj-kondo/config.edn" = { source = ./clj-kondo.edn; };
    ".zsh.d" = {
      source = ./.zsh.d;
      recursive = true;
    };
    "broot/conf.hjson" = { source = ./broot.hjson; };
    ".lsp/config.edn" = { source = ./lsp.edn; };
    htop = {
      source = ./htoprc;
      target = "htop/htoprc";
    };
    alacritty = lib.mkIf pkgs.stdenvNoCC.isDarwin {
      source = ./alacritty;
      recursive = true;
    };
    clojure = {
      source = inputs.clojure-deps-edn;
      target = "./clojure";
      recursive = true;
    };
  };
}
