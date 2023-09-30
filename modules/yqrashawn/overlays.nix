{ inputs, lib, pkgs, ... }:
let
  overlays = final: prev:
    import ../../overlays/default.nix { inherit final prev pkgs; };
in {
  nixpkgs.overlays = [
    overlays
    inputs.emacs-overlay.overlays.default
    (final: prev: {
      emacs29 = prev.emacs-git.overrideAttrs (old: {
        name = "emacs29";
        version = "29.0-${inputs.emacs-custom-src.shortRev}";
        src = inputs.emacs-custom-src;
        patches = [ ];
      });
    })
    # channels
    (final: prev: {
      # expose other channels via overlays
      stable = import inputs.stable { system = prev.system; };
      small = import inputs.small { system = prev.system; };
      darwins = import inputs.darwin-stable { system = prev.system; };
      masters = import inputs.nixpkgs-master { system = prev.system; };
      own = import inputs.own { system = prev.system; };
      # old = import inputs.old { system = prev.system; };
      # emacsMacport = import overlays.emacsMacport { system = prev.system; };
    })
  ];
}
