{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  home = "${if pkgs.stdenvNoCC.isDarwin then "/Users" else "/home"}/${config.user.name}";
  overlays = final: prev: import ../../overlays/default.nix { inherit final prev pkgs; };
in
{
  nixpkgs.overlays = [
    overlays
    inputs.zed.overlays.default
    inputs.emacs-lsp-booster.overlays.default
    inputs.nix-openclaw.overlays.default
    inputs.clojure-lsp.overlays.default
    inputs.emacs-overlay.overlays.default
    (final: prev: {
      emacs30-overlay =
        (prev.emacs-git.override {
          withNS = true;
          withSQLite3 = true;
          withWebP = true;
          withImageMagick = true;
          # withXwidgets = true;
          # withNativeCompilation = false;
          withNativeCompilation = true;
          withTreeSitter = true;
          withJansson = true;
          withMailutils = true;
          withCsrc = true;
        }).overrideAttrs
          (old: {
            name = "emacs30";
            version = "30.0-${inputs.emacs-custom-src.shortRev}";
            src = inputs.emacs-custom-src;
            patches = old.patches ++ [ ];
            buildInputs = old.buildInputs ++ [ ];
            configureFlags = old.configureFlags ++ [
              "--with-xwidgets"
            ];
            # withMacport = true;
            # macportVersion = "master";
            # webkitgtk = true;
            # texinfo = true;
            # autoreconfHook = true;
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
