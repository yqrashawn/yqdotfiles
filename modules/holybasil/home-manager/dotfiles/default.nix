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
    prettier = {
      source = ./.prettierrc;
      target = ".perttierrc";
    };
    textlint = {
      source = ./.textlintrc;
      target = ".textlintrc";
    };
    nvm = {
      source = ./.nvmrc;
      target = ".nvmrc";
    };
    shadow-cljs = {
      source = ./shadow-cljs.edn;
      target = ".shadow-cljs/config.edn";
    };
    gitignoreglobal = {
      source = ./gitignore_global;
      target = ".gitignore_global";
    };
    mdl = {
      source = ./.mdlrc;
      target = ".mdlrc";
    };
    asdf-java = {
      source = inputs.asdf-java;
      target = "./.asdf/plugins/java";
      recursive = true;
    };
    asdf-clojure = {
      source = inputs.asdf-clojure;
      target = "./.asdf/plugins/clojure";
      recursive = true;
    };
    asdf-nodejs = {
      source = inputs.asdf-nodejs;
      target = "./.asdf/plugins/nodejs";
      recursive = true;
    };
    asdf-ruby = {
      source = inputs.asdf-ruby;
      target = "./.asdf/plugins/ruby";
      recursive = true;
    };
    asdf-python = {
      source = inputs.asdf-python;
      target = "./.asdf/plugins/python";
      recursive = true;
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
