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

      (pkgs.tree-sitter.buildGrammar {
        language = "doxygen";
        version = "0-unstable-2024";
        src = pkgs.fetchFromGitHub {
          owner = "tree-sitter-grammars";
          repo = "tree-sitter-doxygen";
          rev = "ccd998f378c3f9345ea4eeb223f56d7b84d16687";
          hash = "sha256-Yh6FaRvWmeqnSnBgOojWbs1wJaeEoNJlvSEqgzjGh7o=";
        };
      })

      (emacs.pkgs.callPackage ./fuzzy-matcher.nix { })

      (pkgs.stdenv.mkDerivation {
        name = "hexrgb.el";
        src = inputs.hexrgb;
        buildPhase = "${emacs}/bin/emacs -Q -nw -batch -f batch-byte-compile hexrgb.el";
        installPhase = "mkdir -p $out/share/emacs/site-lisp && install *.el* $out/share/emacs/site-lisp";
      })
    ];
}
