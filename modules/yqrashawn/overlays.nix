{ inputs, lib, ... }: {
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
    # channels
    (final: prev: {
      # expose other channels via overlays
      stable = import inputs.stable { system = prev.system; };
      small = import inputs.small { system = prev.system; };
    })
  ];
}
