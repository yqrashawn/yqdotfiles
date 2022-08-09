{ coreutils, lib, stdenv, fetchurl, openssl, libtool, libkrb5, db, gettext, pam
, fixDarwinDylibNames, buildPackages, zlib, automake, autoconf, cyrus_sasl
, pkg-config }:

with lib;
stdenv.mkDerivation rec {
  pname = "cyrus-sasl-xoauth2";
  version = "36aabca54fd65c8fa7a707cb4936751599967904";

  src = fetchurl {
    url =
      "https://github.com/moriyoshi/cyrus-sasl-xoauth2/archive/${version}.tar.gz";
    sha256 = "sha256-B9enfHwn+DkjuDDL3UFWAtaA6F9Liiva9hYyKuoRuN0=";
  };

  depsBuildBuild = [ buildPackages.stdenv.cc ];
  configureFlags = [
    "--prefix=${placeholder "out"}"
    "--with-cyrus-sasl=${getLib cyrus_sasl}"
  ];
  nativeBuildInputs = [ libtool automake autoconf pkg-config ]
    ++ lib.optional stdenv.hostPlatform.isDarwin fixDarwinDylibNames;
  buildInputs = [ openssl db cyrus_sasl zlib gettext libkrb5 ]
    ++ lib.optional stdenv.isLinux pam;

  preConfigure = ''
    LIBTOOLIZE=libtoolize ./autogen.sh
  '';
  installPhase = ''
    ${coreutils}/bin/mkdir -p $out/lib/sasl2
    # ${coreutils}/bin/mkdir -p ${getLib cyrus_sasl}/lib/sasl2
    ./libtool --mode=install ${coreutils}/bin/install -c .libs/libxoauth2.0.so $out/lib/libxoauth2.0.so
    ./libtool --mode=install ${coreutils}/bin/install -c .libs/libxoauth2.0.so $out/lib/sasl2/libxoauth2.0.so
    # ./libtool --mode=install ${coreutils}/bin/install -c .libs/libxoauth2.0.so ${
      getLib cyrus_sasl
    }/lib/sasl2/libxoauth2.0.so
  '';

  meta = {
    homepage =
      "https://github.com/moriyoshi/cyrus-sasl-xoauth2/tree/master#readme";
    description = "XOAUTH2 mechanism plugin for cyrus-sasl ";
    platforms = platforms.unix;
    license = licenses.mit;
  };
}
