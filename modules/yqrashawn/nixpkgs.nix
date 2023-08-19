{ inputs, config, lib, pkgs, ... }: {
  nixpkgs = { config = import ./config.nix; };

  nix = {
    package = pkgs.nix;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
    settings = {
      trusted-users = [ "${config.user.name}" "root" "@admin" "@wheel" ];
      max-jobs = 8;
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://rashawn.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "rashawn.cachix.org-1:7IFMjxmyIGgqR6v7iqpziOvPQ784z6+2EeW6bI46DIs="
      ];
    };

    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
    };
    # readOnlyStore = true;
    nixPath = builtins.map
      (source: "${source}=/etc/${config.environment.etc.${source}.target}") [
        "home-manager"
        "nixpkgs"
        "stable"
      ];

    registry = {
      nixpkgs = {
        from = {
          id = "nixpkgs";
          type = "indirect";
        };
        flake = inputs.nixpkgs;
      };

      stable = {
        from = {
          id = "stable";
          type = "indirect";
        };
        flake = inputs.stable;
      };
    };
  };
}
