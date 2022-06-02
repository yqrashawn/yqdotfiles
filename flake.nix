{
  description = "nix system configurations";

  nixConfig = {
    substituters =
      [ "https://cache.nixos.org" "https://nix-community.cachix.org" ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    stable.url = "github:nixos/nixpkgs/nixos-21.11";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    sops-nix.url = "github:Mic92/sops-nix";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    comma = {
      url = "github:nix-community/comma";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    darwin = {
      url = "github:LnL7/nix-darwin";
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

    zsh-f-sy-h = {
      url = "github:z-shell/F-Sy-H";
      flake = false;
    };
    zsh-abbrev-alias = {
      url = "github:momo-lab/zsh-abbrev-alias";
      flake = false;
    };
    zsh-alias-tips = {
      url = "github:djui/alias-tips";
      flake = false;
    };
  };

  outputs =
    inputs@{ self, nixpkgs, darwin, home-manager, sops-nix, flake-utils, ... }:
    let
      inherit (darwin.lib) darwinSystem;
      inherit (nixpkgs.lib) nixosSystem;
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
      mkhDarwinConfig = { system, nixpkgs ? inputs.nixpkgs
        , stable ? inputs.stable, baseModules ? [
          home-manager.darwinModules.home-manager
          ./modules/holybasil/darwin
        ], extraModules ? [ ] }:
        darwinSystem {
          inherit system;
          modules = baseModules ++ extraModules;
          specialArgs = { inherit inputs nixpkgs stable; };
        };

      # generate a base nixos configuration with the
      # specified overlays, hardware modules, and any extraModules applied
      mkNixosConfig = { system ? "x86_64-linux", nixpkgs ? inputs.nixos-unstable
        , stable ? inputs.stable, hardwareModules, baseModules ? [
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
          ./modules/yqrashawn/nixos
        ], extraModules ? [ ] }:
        nixosSystem {
          inherit system;
          modules = baseModules ++ hardwareModules ++ extraModules;
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
        }) nixpkgs.lib.platforms.darwin) ++
        # linux checks
        (map (system: {
          name = system;
          value = {
            nixos = self.nixosConfigurations.phil.config.system.build.toplevel;
            # server = self.homeConfigurations.server.activationPackage;
          };
        }) nixpkgs.lib.platforms.linux));

      darwinConfigurations = {
        yqrashawn = mkDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [
            ./profiles/yqrashawn.nix
            ./modules/yqrashawn/darwin/apps.nix
            ./modules/yqrashawn/darwin/apps-minimal.nix
            { homebrew.brewPrefix = "/opt/homebrew/bin"; }
          ];
        };
        holybasil = mkhDarwinConfig {
          system = "aarch64-darwin";
          extraModules = [
            ./profiles/holybasil.nix
            ./modules/holybasil/darwin/apps-minimal.nix
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

      nixosConfigurations = {
        phil = mkNixosConfig {
          hardwareModules = [
            ./modules/yqrashawn/hardware/phil.nix
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t460s
          ];
          extraModules = [ ./profiles/yqrashawn.nix ];
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
        lib = nixpkgs.lib.extend (self: super: {
          my = import ./lib {
            inherit pkgs inputs;
            lib = self;
          };
        });

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ inputs.devshell.overlay ];
        };
        pyEnv = (pkgs.python3.withPackages
          (ps: with ps; [ black pylint typer colorama shellingham ]));
        sysdo = pkgs.writeShellScriptBin "sysdo" ''
          cd $PRJ_ROOT && ${pyEnv}/bin/python3 bin/do.py $@
        '';
      in {
        devShell = pkgs.devshell.mkShell {
          packages = with pkgs; [ nixfmt pyEnv rnix-lsp stylua treefmt ];
          commands = [{
            name = "sysdo";
            package = sysdo;
            category = "utilities";
            help = "perform actions on this repository";
          }];
        };
      });
}
