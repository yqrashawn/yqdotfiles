{ lib, fetchurl, fetchpatch, fetchFromGitHub, stdenv, lispPackages, makeWrapper
, wrapGAppsHook, gst_all_1, glib, gdk-pixbuf, cairo, mailcap, pango, gtk3
, glib-networking, gsettings-desktop-schemas, xclip, enchant, libressl
, webkitgtk, sbcl, gobject-introspection, pkg-config, libfixposix, lisp-nyxt }:

with lib;
stdenv.mkDerivation rec {
  pname = "nyxt-m1-mac";
  inherit (lisp-nyxt.meta) version;

  src = lisp-nyxt;

  nativeBuildInputs = [ makeWrapper wrapGAppsHook libressl webkitgtk sbcl ];
  gstBuildInputs = with gst_all_1; [
    gstreamer
    gst-plugins-base
    gst-plugins-good
    gst-plugins-bad
    gst-plugins-ugly
    gst-libav
  ];
  buildInputs = [
    # glib
    # gdk-pixbuf
    # cairo
    # mailcap
    # pango
    # gtk3
    # glib-networking
    # gsettings-desktop-schemas
    # xclip
    # enchant
    gobject-introspection
    pkg-config
    enchant.out
    gsettings-desktop-schemas.out
    glib-networking.out
    pango.out
    cairo.out
    gdk-pixbuf.out
    gtk3.out
    glib.out
    libfixposix.out
    webkitgtk
  ] ++ gstBuildInputs;

  GST_PLUGIN_SYSTEM_PATH_1_0 =
    lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" gstBuildInputs;

  binPath = lib.optionals (!stdenv.isDarwin) [ xclip ];

  dontWrapGApps = true;
  # installPhase = ''
  #   mkdir -p $out/share/applications/
  #   sed "s/VERSION/$version/" $src/lib/common-lisp/nyxt/assets/nyxt.desktop > $out/share/applications/nyxt.desktop
  #   for i in 16 32 128 256 512; do
  #     mkdir -p "$out/share/icons/hicolor/''${i}x''${i}/apps/"
  #     cp -f $src/lib/common-lisp/nyxt/assets/nyxt_''${i}x''${i}.png "$out/share/icons/hicolor/''${i}x''${i}/apps/nyxt.png"
  #   done

  #   mkdir -p $out/bin && makeWrapper $src/bin/nyxt $out/bin/nyxt \
  #     --prefix GST_PLUGIN_SYSTEM_PATH_1_0 : "${GST_PLUGIN_SYSTEM_PATH_1_0}" \
  #     --argv0 nyxt "''${gappsWrapperArgs[@]}"
  # '';

  installPhase = ''
    runHook preInstall
  '' + (if stdenv.isDarwin then ''
    mkdir -p $out/bin $out/Applications/Nyxt.app/Contents
    pushd $out/Applications/Nyxt.app/Contents
    install -Dm644 $src/lib/common-lisp/nyxt/assets/Info.plist Info.plist
    install -Dm644 $src/lib/common-lisp/nyxt/assets/nyxt.icns Resources/nyxt.icns
    install -Dm755 $src/bin/nyxt MacOS/nyxt
    popd
    gappsWrapperArgsHook # FIXME: currently runs at preFixup
    wrapGApp $out/Applications/Nyxt.app/Contents/MacOS/nyxt \
      --prefix PATH : "${lib.makeBinPath binPath}" \
      --argv0 nyxt
    ln -s $out/Applications/Nyxt.app/Contents/MacOS/nyxt $out/bin/nyxt
  '' else ''
    mkdir -p $out/share/applications/
    sed "s/VERSION/$version/" $src/lib/common-lisp/nyxt/assets/nyxt.desktop > $out/share/applications/nyxt.desktop
    for i in 16 32 128 256 512; do
      mkdir -p "$out/share/icons/hicolor/''${i}x''${i}/apps/"
      cp -f $src/lib/common-lisp/nyxt/assets/nyxt_''${i}x''${i}.png "$out/share/icons/hicolor/''${i}x''${i}/apps/nyxt.png"
    done
    install -Dm755 $src/bin/nyxt $out/bin/nyxt
    gappsWrapperArgsHook # FIXME: currently runs at preFixup
    wrapGApp $out/bin/nyxt \
      --prefix PATH : "${lib.makeBinPath binPath}" \
      --argv0 nyxt
  '') + ''
    runHook postInstall
  '';

  checkPhase = ''
    $out/bin/nyxt -h
  '';

  meta = with lib; {
    description =
      "Infinitely extensible web-browser (with Lisp development files using WebKitGTK platform port)";
    homepage = "https://nyxt.atlas.engineer";
    license = licenses.bsd3;
    maintainers = with maintainers; [ lewo payas ];
    platforms = platforms.all;
  };
}
