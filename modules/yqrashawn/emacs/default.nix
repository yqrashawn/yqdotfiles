{
  pkgs,
  emacs,
  inputs,
  ...
}:

pkgs.emacsWithPackagesFromUsePackage {
  package = pkgs.emacs30-overlay;
  config = "./empty.el";
  defaultInitFile = false;
  extraEmacsPackages =
    epkgs: with epkgs; [
      parinfer-rust-mode
      jinx
      hotfuzz
      fuz
      treesit-grammars.with-all-grammars

      (emacs.pkgs.callPackage ./fuzzy-matcher.nix { })

      (pkgs.stdenv.mkDerivation {
        name = "hexrgb.el";
        src = inputs.hexrgb;
        buildPhase = "${emacs}/bin/emacs -Q -nw -batch -f batch-byte-compile hexrgb.el";
        installPhase = "mkdir -p $out/share/emacs/site-lisp && install *.el* $out/share/emacs/site-lisp";
      })
    ];
}
