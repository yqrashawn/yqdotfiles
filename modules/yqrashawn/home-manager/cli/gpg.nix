{ config, lib, pkgs, ... }:

{
  programs.gpg = {
    enable = true;
    # use gnupg 2.4.0
    # until this is fixed https://dev.gnupg.org/T6481
    # package = pkgs.darwins.gnupg;
    settings = { pinentry-mode = "loopback"; };
  };
}
