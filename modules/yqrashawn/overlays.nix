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
    inputs.clojure-lsp.overlays.default
    inputs.emacs-overlay.overlays.default
    (final: prev: {
      emacs30-overlay = prev.emacs-git.overrideAttrs (old: {
        name = "emacs30";
        version = "30.0-${inputs.emacs-custom-src.shortRev}";
        src = inputs.emacs-custom-src;
        patches = # old.patches ++
          [
            # "${home}/.nixpkgs/modules/yqrashawn/emacs-patches/system-appearance.patch"
            # "${home}/.nixpkgs/modules/yqrashawn/emacs-patches/round-undecorated-frame.patch"
            # "${home}/.nixpkgs/modules/yqrashawn/emacs-patches/poll.patch"
            # "${home}/.nixpkgs/modules/yqrashawn/emacs-patches/fix-window-role.patch"
          ];
        buildInputs = old.buildInputs
          ++ [ pkgs.darwin.apple_sdk.frameworks.WebKit ];
        configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
        # withMacport = true;
        withNS = true;
        # macportVersion = "master";
        withSQLite3 = true;
        withWebP = true;
        withImageMagick = true;
        # withXwidgets = true;
        # nativeComp = true;
        withNativeCompilation = true;
        withTreeSitter = true;
        webkitgtk = true;
        # texinfo = true;
        # autoreconfHook = true;
        withCsrc = true;
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
