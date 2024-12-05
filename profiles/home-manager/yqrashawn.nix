{ config, lib, pkgs, ... }:
let
  email = "namy.19@gmail.com";
  username = "yqrashawn";
in {
  home.sessionVariables = rec {
    NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    SSL_CERT_FILE = NIX_SSL_CERT_FILE;
    REQUESTS_CA_BUNDLE = NIX_SSL_CERT_FILE;
    PIP_CERT = NIX_SSL_CERT_FILE;
    GIT_SSL_CAINFO = NIX_SSL_CERT_FILE;
    # NODE_EXTRA_CA_CERTS = NIX_SSL_CERT_FILE;
  };
  programs.git = {
    userEmail = email;
    userName = username;
    signing = {
      key = email;
      signByDefault = true;
    };
    extraConfig.github.user = username;
  };
}
