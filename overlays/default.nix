{ final, prev, pkgs ? final, ... }:
let
  cyrus-sasl-xoauth2 = pkgs.callPackage ./cyrus-sasl-xoauth2.nix { };
  cyrus-sasl = pkgs.callPackage ./cyrus-sasl.nix { };
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
}
