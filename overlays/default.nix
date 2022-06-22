{ final, prev, pkgs ? final, ... }: rec {
  emacsMacport = pkgs.callPackage ./emacs-macport.nix {
    inherit (pkgs.darwin.apple_sdk.frameworks)
      AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit Metal
      ImageCaptureCore GSS ImageIO;
    inherit (pkgs.darwin) sigtool;
  };
}
