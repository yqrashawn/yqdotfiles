{ config, lib, pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    package = pkgs.wrapMpv (pkgs.mpv-unwrapped) { youtubeSupport = true; };
  };
}
