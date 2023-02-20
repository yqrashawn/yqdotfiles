{ lib, fetchurl, lispPackages }:

with lib;
lispPackages.buildLispPackage rec {
  baseName = "enchant";
  version = "cl-20211209-git";

  description = "Programming interface for Enchant spell-checker library";

  deps = [
    lispPackages.alexandria
    lispPackages.babel
    lispPackages.cffi
    lispPackages.trivial-features
  ];

  src = fetchurl {
    url =
      "http://beta.quicklisp.org/archive/cl-enchant/2021-12-09/cl-enchant-20211209-git.tgz";
    sha256 = "1j9qliyxfjfz4bbc6snysccnmmk2d2y8kb613rna239dh5g6c03c";
  };

  packageName = "enchant";

  asdFilesToKeep = [ "enchant.asd" ];
  overrides = x: x;
}
