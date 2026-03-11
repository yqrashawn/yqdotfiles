{ inputs, config, lib, pkgs, ... }:
let
  home = "${
      if pkgs.stdenvNoCC.isDarwin then "/Users" else "/home"
    }/${config.user.name}";
  overlays = final: prev:
    import ../../overlays/default.nix { inherit final prev pkgs; };
in {
  nixpkgs.overlays = [
    overlays
    inputs.zed.overlays.default
    inputs.emacs-lsp-booster.overlays.default
    # inputs.nix-openclaw.overlays.default
    (final: prev: if prev.stdenv.hostPlatform.isAarch64 then
      inputs.clojure-lsp.overlays.default final prev
    else {})
    inputs.emacs-overlay.overlays.default
    (final: prev: {
      emacs30-overlay = (prev.emacs-git.override {
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
      }).overrideAttrs (old: {
        name = "emacs30";
        version = "30.0-${inputs.emacs-custom-src.shortRev}";
        src = inputs.emacs-custom-src;
        patches = old.patches ++ [ ];
        buildInputs = old.buildInputs ++ [ ];
        configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
        # withMacport = true;
        # macportVersion = "master";
        # webkitgtk = true;
        # texinfo = true;
        # autoreconfHook = true;
      });
    })
    # rtk - CLI proxy that reduces LLM token consumption
    (final: prev: {
      rtk = prev.stdenv.mkDerivation rec {
        pname = "rtk";
        version = "0.27.2";
        src = prev.fetchurl {
          url =
            "https://github.com/rtk-ai/rtk/releases/download/v${version}/rtk-${
              if prev.stdenv.hostPlatform.isAarch64 then "aarch64" else "x86_64"
            }-apple-darwin.tar.gz";
          sha256 = if prev.stdenv.hostPlatform.isAarch64 then
            "sha256-p2XccLukutDpFu0v2ssDz9nZBqGulPxrXuyXFR3MGhc="
          else
            "0m86ybd3jb0gln51811rhn7lhj3i47maif6r5aygbp63sg1789r3";
        };
        sourceRoot = ".";
        dontBuild = true;
        installPhase = ''
          install -Dm755 rtk $out/bin/rtk
        '';
        meta = with prev.lib; {
          description =
            "CLI proxy that reduces LLM token consumption by 60-90% on common dev commands";
          homepage = "https://github.com/rtk-ai/rtk";
          platforms = platforms.darwin;
        };
      };
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
