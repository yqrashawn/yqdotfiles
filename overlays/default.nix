{ final, prev, pkgs ? final, ... }:
let
  cyrus-sasl-xoauth2 = pkgs.callPackage ./cyrus-sasl-xoauth2.nix { };
  cyrus-sasl = pkgs.callPackage ./cyrus-sasl.nix { };
  webkitgtk-overlay = pkgs.callPackage ./webkitgtk.nix {
    harfbuzz = pkgs.harfbuzzFull;
    inherit (pkgs.gst_all_1) gst-plugins-base gst-plugins-bad;
    inherit (pkgs.darwin) apple_sdk;
  };
  cl-enchant-overlay = pkgs.callPackage ./cl-enchant.nix { };
  lisp-nyxt-overlay = pkgs.callPackage ./lisp-nyxt.nix {
    webkitgtk = webkitgtk-overlay;
    cl-enchant = cl-enchant-overlay;
  };
  nyxt-overlay = pkgs.callPackage ./nyxt.nix {
    webkitgtk = webkitgtk-overlay;
    lisp-nyxt = lisp-nyxt-overlay;
  };
in rec {
  emacsMacport = pkgs.callPackage ./emacs-macport.nix {
    inherit (pkgs.darwin.apple_sdk.frameworks)
      AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit Metal
      ImageCaptureCore GSS ImageIO;
    inherit (pkgs.darwin) sigtool;
  };
  cyrus_sasl_xoauth2 = cyrus-sasl-xoauth2;
  isync-isync = pkgs.callPackage ./isync-isync.nix {
    inherit (pkgs.darwin.apple_sdk.frameworks) Security;
    cyrus_sasl = cyrus-sasl;
  };
  webkitgtk = webkitgtk-overlay;
  nyxt = nyxt-overlay;
}
