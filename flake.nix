{
  description = "nix system configurations";

  nixConfig = {
    substituters = [
      "https://rashawn.cachix.org"
      "https://nix-cache.status.im"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    trusted-substituters = [
      "https://rashawn.cachix.org"
      "https://nix-cache.status.im"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "rashawn.cachix.org-1:7IFMjxmyIGgqR6v7iqpziOvPQ784z6+2EeW6bI46DIs="
      "nix-cache.status.im-1:x/93lOfLU+duPplwMSBR+OlY4+mo+dCN7n0mr4oPwgY="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin-stable.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";
    stable.url = "github:nixos/nixpkgs/nixos-23.05";
    own.url = "github:yqrashawn/nixpkgs/master";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # nixpkgs.follows = "nixpkgs-unstable";
    nixpkgs.url= "github:NixOS/nixpkgs/nixpkgs-unstable";
    clojure-lsp.url = "github:clojure-lsp/clojure-lsp";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    sops-nix.url = "github:Mic92/sops-nix";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-custom-src = {
      url = "github:emacs-mirror/emacs/emacs-30";
      flake = false;
    };
    atuin = {
      url = "github:atuinsh/atuin";
      flake = true;
    };
    comma = {
      url = "github:nix-community/comma";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    zed = {
      url = "github:zed-industries/zed";
      # inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    clojure-deps-edn = {
      url = "github:practicalli/clojure-deps-edn";
      flake = false;
    };
    spacehammer = {
      url = "github:agzam/spacehammer";
      flake = false;
    };
    forgit = {
      url = "github:wfxr/forgit";
      flake = false;
    };
    zsh-alias-tips = {
      url = "github:djui/alias-tips";
      flake = false;
    };
    tmux-conf = {
      url = "github:gpakosz/.tmux";
      flake = false;
    };
    zsh-autoquoter = {
      url = "github:ianthehenry/zsh-autoquoter";
      flake = false;
    };
    asdf-java = {
      url = "github:halcyon/asdf-java";
      flake = false;
    };
    asdf-clojure = {
      url = "github:halcyon/asdf-clojure";
      flake = false;
    };
    asdf-nodejs = {
      url = "github:asdf-vm/asdf-nodejs";
      flake = false;
    };
    asdf-python = {
      url = "github:danhper/asdf-python";
      flake = false;
    };
    asdf-ruby = {
      url = "github:asdf-vm/asdf-ruby";
      flake = false;
    };
    asdf-golang = {
      url = "github:kennyp/asdf-golang";
      flake = false;
    };
    # https://github.com/smashedtoatoms/asdf-sbcl
    asdf-sbcl = {
      url = "github:smashedtoatoms/asdf-sbcl";
      flake = false;
    };
    asdf-deno = {
      url = "github:asdf-community/asdf-deno";
      flake = false;
    };
    asdf-crystal = {
      url = "github:asdf-community/asdf-crystal";
      flake = false;
    };
    asdf-elixir = {
      url = "github:asdf-vm/asdf-elixir";
      flake = false;
    };
    asdf-haskell = {
      url = "github:asdf-community/asdf-haskell";
      flake = false;
    };
    asdf-janet = {
      url = "github:Jakski/asdf-janet";
      flake = false;
    };
    asdf-lua = {
      url = "github:Stratus3D/asdf-lua";
      flake = false;
    };
    asdf-ocaml = {
      url = "github:asdf-community/asdf-ocaml";
      flake = false;
    };
    asdf-chezscheme = {
      url = "github:asdf-community/asdf-chezscheme";
      flake = false;
    };
    asdf-bun = {
      url = "github:cometkim/asdf-bun";
      flake = false;
    };
    asdf-pnpm = {
      url = "github:jonathanmorley/asdf-pnpm";
      flake = false;
    };
    asdf-racket = {
      url = "github:asdf-community/asdf-racket";
      flake = false;
    };
    asdf-scala = {
      url = "github:asdf-community/asdf-scala";
      flake = false;
    };
    asdf-hashicorp = {
      url = "github:asdf-community/asdf-hashicorp";
      flake = false;
    };
    asdf-solidity = {
      url = "github:diegodorado/asdf-solidity";
      flake = false;
    };
    asdf-nim = {
      url = "github:asdf-community/asdf-nim";
      flake = false;
    };
    asdf-v = {
      url = "github:ndac-todoroki/asdf-v";
      flake = false;
    };
    asdf-yarn = {
      url = "github:twuni/asdf-yarn";
      flake = false;
    };
    asdf-zig = {
      url = "github:cheetah/asdf-zig";
      flake = false;
    };
    asdf-ninja = {
      url = "github:asdf-community/asdf-ninja";
      flake = false;
    };
    asdf-babashka = {
      url = "github:pitch-io/asdf-babashka";
      flake = false;
    };
    asdf-flyctl = {
      url = "github:chessmango/asdf-flyctl";
      flake = false;
    };
    asdf-uv = {
      url = "github:asdf-community/asdf-uv";
      flake = false;
    };
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    hexrgb = {
      type = "github";
      owner = "emacsmirror";
      repo = "hexrgb";
      flake = false;
    };
  };

  outputs =
    inputs@{ self, nixpkgs, darwin, home-manager, sops-nix, flake-utils, ... }:
    let
      inherit (darwin.lib) darwinSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      inherit (builtins) listToAttrs map;

      isDarwin = system: (builtins.elem system nixpkgs.lib.platforms.darwin);
      homePrefix = system: if isDarwin system then "/Users" else "/home";

      # generate a base darwin configuration with the
      # specified hostname, overlays, and any extraModules applied
      mkDarwinConfig = { system, nixpkgs ? inputs.nixpkgs
        , stable ? inputs.stable, baseModules ? [
          home-manager.darwinModules.home-manager
          ./modules/yqrashawn/darwin
        ], extraModules ? [ ] }:
        darwinSystem {
          inherit system;
          modules = baseModules ++ extraModules;
          specialArgs = { inherit inputs nixpkgs stable; };
        };

      # generate a home-manager configuration usable on any unix system
      # with overlays and any extraModules applied
      mkHomeConfig = { username, system ? "x86_64-linux"
        , nixpkgs ? inputs.nixpkgs, stable ? inputs.stable, baseModules ? [
          ./modules/yqrashawn/home-manager
          {
            home.sessionVariables = {
              NIX_PATH =
                "nixpkgs=${nixpkgs}:stable=${stable}\${NIX_PATH:+:}$NIX_PATH";
            };
          }
        ], extraModules ? [ ] }:
        homeManagerConfiguration rec {
          inherit system username;
          homeDirectory = "${homePrefix system}/${username}";
          extraSpecialArgs = { inherit inputs nixpkgs stable; };
          configuration = {
            imports = baseModules ++ extraModules
              ++ [ ./modules/yqrashawn/overlays.nix ];
          };
        };
    in {
      checks = listToAttrs (
        # darwin checks
        (map (system: {
          name = system;
          value = {
            darwin =
              self.darwinConfigurations.yqrashawn-intel.config.system.build.toplevel;
            # darwinServer =
            #   self.homeConfigurations.darwinServer.activationPackage;
          };
        }) nixpkgs.lib.platforms.darwin));

      darwinConfigurations = {
        yqrashawn = mkDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [
            ./profiles/yqrashawn.nix
            ./modules/yqrashawn/darwin/apps.nix
            { homebrew.brewPrefix = "/opt/homebrew/bin"; }
          ];
        };
        mini = mkDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [
            ./profiles/yqrashawn.nix
            ./modules/yqrashawn/darwin/apps.nix
            ./modules/yqrashawn/mini.nix
            { homebrew.brewPrefix = "/opt/homebrew/bin"; }
          ];
        };
        studio = mkDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [
            ./profiles/yqrashawn.nix
            ./modules/yqrashawn/darwin/apps.nix
            ./modules/yqrashawn/studio.nix
            { homebrew.brewPrefix = "/opt/homebrew/bin"; }
          ];
        };
        mbp = mkDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [
            ./profiles/yqrashawn.nix
            ./modules/yqrashawn/darwin/apps.nix
            ./modules/yqrashawn/mbp.nix
            { homebrew.brewPrefix = "/opt/homebrew/bin"; }
          ];
        };
        holybasil = mkDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [
            ./profiles/holybasil.nix
            ./modules/yqrashawn/darwin/happs.nix
            ./modules/yqrashawn/hmbp.nix
            { homebrew.brewPrefix = "/opt/homebrew/bin"; }
          ];
        };
        yqrashawn-intel = mkDarwinConfig {
          system = "x86_64-darwin";
          extraModules = [
            ./profiles/yqrashawn.nix
            ./modules/yqrashawn/darwin/apps.nix
            { homebrew.brewPrefix = "/usr/local/bin"; }
          ];
        };
        work = mkDarwinConfig {
          system = "x86_64-darwin";
          extraModules =
            [ ./profiles/work.nix ./modules/yqrashawn/darwin/apps-minimal.nix ];
        };
      };

      # homeConfigurations = {
      #   server = mkHomeConfig {
      #     username = "yqrashawn";
      #     extraModules = [ ./profiles/home-manager/yqrashawn.nix ];
      #   };
      #   darwinServer = mkHomeConfig {
      #     username = "yqrashawn";
      #     system = "x86_64-darwin";
      #     extraModules = [ ./profiles/home-manager/yqrashawn.nix ];
      #   };
      #   darwinServerM1 = mkHomeConfig {
      #     username = "yqrashawn";
      #     system = "aarch64-darwin";
      #     extraModules = [ ./profiles/home-manager/yqrashawn.nix ];
      #   };
      #   workServer = mkHomeConfig {
      #     username = "yqrashawn";
      #     extraModules = [ ./profiles/home-manager/work.nix ];
      #   };
      #   vagrant = mkHomeConfig {
      #     username = "vagrant";
      #     extraModules = [ ./profiles/home-manager/yqrashawn.nix ];
      #   };
      # };
    } //
    # add a devShell to this flake
    eachDefaultSystem (system:
      let
        inherit (lib.my) mapModules mapModulesRec mapHosts;
        lib = inputs.stable.lib.extend (self: super: {
          my = import ./lib {
            inherit pkgs inputs;
            lib = self;
          };
        });

        pkgs = import inputs.stable {
          inherit system;
          overlays = [ inputs.devshell.overlays.default ];
        };
        pyEnv = (pkgs.python3.withPackages
          (ps: with ps; [ black pylint typer colorama shellingham ]));
        sysdo = pkgs.writeShellScriptBin "sysdo" ''
          cd $PRJ_ROOT && ${pyEnv}/bin/python3 bin/do.py $@
        '';
      in {
        devShell = pkgs.devshell.mkShell {
          packages = with pkgs; [ nixfmt pyEnv stylua treefmt ];
          commands = [{
            name = "sysdo";
            package = sysdo;
            category = "utilities";
            help = "perform actions on this repository";
          }];
        };
      });
}
