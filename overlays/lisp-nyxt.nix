{ lib, lispPackages, fetchFromGitHub, enchant, libressl, webkitgtk, sbcl
, cl-enchant }:

with lib;
lispPackages.buildLispPackage rec {
  baseName = "nyxt";
  version = "3-pre-release-3";

  description = "Browser";

  overrides = x: {
    # postInstall = ''
    #   echo "Building nyxt binary"
    #   (
    #     source "$out/lib/common-lisp-settings"/*-shell-config.sh
    #     cd "$out/lib/common-lisp"/*/
    #     makeFlags="''${makeFlags:-}"
    #     make LISP=common-lisp.sh NYXT_INTERNAL_QUICKLISP=false PREFIX="$out" $makeFlags all
    #     cp nyxt "$out/bin/nyxt"
    #   )
    #   NIX_LISP_PRELAUNCH_HOOK='
    #     nix_lisp_build_system nyxt/gtk-application \
    #      "(asdf/system:component-entry-point (asdf:find-system :nyxt/gtk-application))" \
    #      "" "(format *error-output* \"Alien objects:~%~s~%\" sb-alien::*shared-objects*)"
    #   ' "$out/bin/nyxt-lisp-launcher.sh"
    #   cp "$out/lib/common-lisp/nyxt/nyxt" "$out/bin/"
    # '';

    # postInstall = ''
    #   echo "Building nyxt binary"
    #   # clear unnecessary environment variables to avoid hitting the limit
    #   env -i \
    #   NIX_LISP="$NIX_LISP" \
    #   NIX_LISP_PRELAUNCH_HOOK='
    #     nix_lisp_build_system nyxt/gtk-application \
    #       "(asdf/system:component-entry-point (asdf:find-system :nyxt/gtk-application))" \
    #       "" \
    #       "(format *error-output* \"Alien objects:~%~s~%\" sb-alien::*shared-objects*)"
    #   ' "$out/bin/${baseName}-lisp-launcher.sh"
    #   mv "$out/lib/common-lisp/${baseName}/nyxt" "$out/bin/"
    # '';

    postInstall = ''
      echo "Building nyxt binary"
      # clear unnecessary environment variables to avoid hitting the limit
      env -i \
      NIX_LISP="$NIX_LISP" \
      NIX_LISP_PRELAUNCH_HOOK='
        nix_lisp_build_system nyxt/gi-gtk-application \
          "(asdf/system:component-entry-point (asdf:find-system :nyxt/gi-gtk-application))" \
          "" \
          "(format *error-output* \"Alien objects:~%~s~%\" sb-alien::*shared-objects*)"
      ' "$out/bin/${baseName}-lisp-launcher.sh"
      mv "$out/lib/common-lisp/${baseName}/nyxt" "$out/bin/"
    '';

    # Prevent nyxt from trying to obtain dependencies as submodules
    makeFlags = [ "NYXT_SUBMODULES=false" ] ++ x.buildFlags or [ ];

    patches = x.patches or [ ] # ++ [
      #   # Work around crash when opening _any_ URL
      #   # https://github.com/atlas-engineer/nyxt/issues/1781
      #   # https://github.com/NixOS/nixpkgs/issues/158005
      #   (fetchpatch {
      #     name = "nyxt-webkit-disable-sandbox.patch";
      #     url =
      #       "https://github.com/atlas-engineer/nyxt/commit/48ac0d8727f1ca1428188a1ab2c05b7be5f6cc51.patch";
      #     sha256 = "0570mcfn5wmjha6jmfdgglp0w5b7rpfnv3flzn77clgbknwbxi0m";
      #   })
      # ]
    ;
  };

  deps = with lispPackages;
    [
      alexandria
      bordeaux-threads
      calispel
      cl-css
      cl-json
      cl-markup
      cl-ppcre
      cl-ppcre-unicode
      cl-prevalence
      closer-mop
      cl-containers
      cl-qrencode
      clss
      cluffer
      moptilities
      dexador
      # enchant
      file-attributes
      iolib
      local-time
      log4cl
      lparallel
      mk-string-metrics
      osicat
      parenscript
      quri
      serapeum
      spinneret
      str
      plump
      swank
      trivia
      trivial-clipboard
      trivial-features
      trivial-garbage
      trivial-package-local-nicknames
      trivial-types
      unix-opts
      cl-html-diff
      hu_dot_dwim_dot_defclass-star
      cl-custom-hash-table
      fset
      cl-cffi-gtk
      cl-webkit2
      cl-gobject-introspection
    ] ++ [ cl-enchant ];

  src = fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "nyxt";
    rev = "${version}";
    sha256 = "12l7ir3q29v06jx0zng5cvlbmap7p709ka3ik6x29lw334qshm9b";
  };

  packageName = "nyxt";

  propagatedBuildInputs = [ enchant libressl.out webkitgtk sbcl ];
}
