{ inputs, lib, pkgs, ... }:
let
  overlays = final: prev:
    import ../../overlays/default.nix { inherit final prev pkgs; };
in {
  nixpkgs.overlays = [
    overlays
    inputs.emacs-overlay.overlay
    # channels
    (final: prev: {
      # expose other channels via overlays
      stable = import inputs.stable { system = prev.system; };
      small = import inputs.small { system = prev.system; };
      # emacsMacport = import overlays.emacsMacport { system = prev.system; };
    })
  ];
}
