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
    # spacehammer = {
    #   source = ./.spacehammer;
    #   target = ".spacehammer";
    #   recursive = true;
    # };
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
    # inputrc = {
    #   srouce = ./.inputrc;
    #   target = ".inputrc";
    # };
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
    zprint = {
      source = ./.zprintrc;
      target = ".zprintrc";
    };
    tern = {
      source = ./.tern-config;
      target = ".tern-config";
    };
    textlint = {
      source = ./.textlintrc;
      target = ".textlintrc";
    };
    # tridactyl = {
    #   source = ./.tridactylrc;
    #   target = ".tridactylrc";
    # };
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
      target = "mail/namy.19@gmail.com/.gmailieer.json";
    };
    gmailieer2 = {
      source = ./gmailieer.2.json;
      target = "mail/yqrashawn@gamil.com/.gmailieer.json";
    };
    shadow-cljs = {
      source = ./shadow-cljs.edn;
      target = ".shadow-cljs/config.edn";
    };
    # gpg-agent-conf = {
    #   source = pkgs.writeTextFile {
    #     name = "gpg-agent.conf";
    #     text = ''
    #       default-cache-ttl 600
    #       max-cache-ttl 7200
    #       allow-emacs-pinentry
    #       allow-loopback-pinentry
    #       enable-ssh-support
    #       pinentry-program ${pkgs.pinentry-emacs}/bin/pinentry-emacs
    #       # pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
    #     '';
    #   };
    #   target = ".gnupg/gpg-agent.conf";
    # };
    lein = {
      source = ./.lein;
      target = ".lein";
      recursive = true;
    };
    osascript = {
      source = ./osascript;
      target = "osascript";
      recursive = true;
    };
    tabnine = {
      source = ./TabNine.toml;
      target = "Library/Preferences/TabNine/TabNine.toml";
    };
    # gitignoreglobal = {
    #   source = ./gitignore_global;
    #   target = ".gitignore_global";
    # };
    husky = {
      source = ./.huskyrc;
      target = ".huskyrc";
    };
    msmtprc = {
      source = ./.msmtprc;
      target = ".msmtprc";
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
    asdf-golang = {
      source = inputs.asdf-golang;
      target = "./.asdf/plugins/golang";
      recursive = true;
    };
    asdf-nim = {
      source = inputs.asdf-nim;
      target = "./.asdf/plugins/nim";
      recursive = true;
    };
    asdf-janet = {
      source = inputs.asdf-janet;
      target = "./.asdf/plugins/janet";
      recursive = true;
    };
    asdf-bun = {
      source = inputs.asdf-bun;
      target = "./.asdf/plugins/bun";
      recursive = true;
    };
    # asdf-yarn = {
    #   source = inputs.asdf-yarn;
    #   target = "./.asdf/plugins/yarn";
    #   recursive = true;
    # };
    asdf-pnpm = {
      source = inputs.asdf-pnpm;
      target = "./.asdf/plugins/pnpm";
      recursive = true;
    };
    asdf-deno = {
      source = inputs.asdf-deno;
      target = "./.asdf/plugins/deno";
      recursive = true;
    };
    asdf-ninja = {
      source = inputs.asdf-ninja;
      target = "./.asdf/plugins/ninja";
      recursive = true;
    };
    asdf-chezscheme = {
      source = inputs.asdf-chezscheme;
      target = "./.asdf/plugins/chezscheme";
      recursive = true;
    };
    asdf-v = {
      source = inputs.asdf-v;
      target = "./.asdf/plugins/v";
      recursive = true;
    };
    asdf-solidity = {
      source = inputs.asdf-solidity;
      target = "./.asdf/plugins/solidity";
      recursive = true;
    };
    asdf-racket = {
      source = inputs.asdf-racket;
      target = "./.asdf/plugins/racket";
      recursive = true;
    };
    asdf-scala = {
      source = inputs.asdf-scala;
      target = "./.asdf/plugins/scala";
      recursive = true;
    };
    asdf-zig = {
      source = inputs.asdf-zig;
      target = "./.asdf/plugins/zig";
      recursive = true;
    };
    asdf-babashka = {
      source = inputs.asdf-babashka;
      target = "./.asdf/plugins/babashka";
      recursive = true;
    };
    asdf-flyctl = {
      source = inputs.asdf-flyctl;
      target = "./.asdf/plugins/flyctl";
      recursive = true;
    };
    asdf-lua = {
      source = inputs.asdf-lua;
      target = "./.asdf/plugins/lua";
      recursive = true;
    };
    asdf-haskell = {
      source = inputs.asdf-haskell;
      target = "./.asdf/plugins/haskell";
      recursive = true;
    };
    asdf-elixir = {
      source = inputs.asdf-elixir;
      target = "./.asdf/plugins/elixir";
      recursive = true;
    };
    asdf-ocaml = {
      source = inputs.asdf-ocaml;
      target = "./.asdf/plugins/ocaml";
      recursive = true;
    };
    asdf-crystal = {
      source = inputs.asdf-crystal;
      target = "./.asdf/plugins/crystal";
      recursive = true;
    };
    asdf-sbcl = {
      source = inputs.asdf-sbcl;
      target = "./.asdf/plugins/sbcl";
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
    # yabai = lib.mkIf pkgs.stdenvNoCC.isDarwin {
    #   source = ./yabai;
    #   recursive = true;
    # };
    # karabiner = {
    #   source = ./karabiner.edn;
    #   target = "karabiner.edn";
    # };
    atuin = {
      source = ./atuin.toml;
      target = "atuin/config.toml";
    };
    kitty-light = {
      source =
        "${pkgs.kitty-themes}/share/kitty-themes/themes/Modus_Operandi.conf";
      target = "kitty/light.conf";
    };
    kitty-light2 = {
      source =
        "${pkgs.kitty-themes}/share/kitty-themes/themes/Modus_Operandi_Faint.conf";
      target = "kitty/light2.conf";
    };
    kitty-dark = {
      source =
        "${pkgs.kitty-themes}/share/kitty-themes/themes/Modus_Vivendi.conf";
      target = "kitty/dark.conf";
    };
    kitty-dark2 = {
      source =
        "${pkgs.kitty-themes}/share/kitty-themes/themes/Modus_Operandi_Faint.conf";
      target = "kitty/dark2.conf";
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
    # "enchant" = {
    #   source = ./enchant;
    #   recursive = true;
    # };
    # "enchant/hunspell" = {
    #   source = ./enchant;
    #   recursive = true;
    # };
    "clj-kondo/config.edn" = { source = ./clj-kondo.edn; };
    ".zsh.d" = {
      source = ./.zsh.d;
      recursive = true;
    };
    "broot/conf.hjson" = { source = ./broot.hjson; };
    ".lsp/config.edn" = { source = ./lsp.edn; };
    alacritty = lib.mkIf pkgs.stdenvNoCC.isDarwin {
      source = ./alacritty;
      recursive = true;
    };
    clojure = {
      source = inputs.clojure-deps-edn;
      target = "./clojure";
      recursive = true;
    };
    "mpv/mpv.conf" = { source = ./mpv.conf; };
    # yarn-global = {
    #   source = ./package.json;
    #   target = "./yarn/global/package.json";
    # };
  };
}
