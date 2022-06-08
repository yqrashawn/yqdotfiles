{ config, lib, pkgs, ... }:

let
  tmux-repo = pkgs.fetchFromGitHub {
    owner = "gpakosz";
    repo = ".tmux";
    rev = "master";
    stripRoot = false;
    sha256 = "sha256-K+qLyNhZP0seJaIbTrmC/jOpYA3rAk7VSwW/IFfP2Z4=";
  };
in {
  home.file.".tmux.conf".text =
    builtins.readFile (tmux-repo + "/.tmux-master/.tmux.conf");
  home.file.tmux-local = {
    source = ./.tmux.conf.local;
    target = "./.tmux.conf.local";
  };
  programs.tmux = {
    enable = true;
    # extraConfig = "source ~/.tmux/.tmux.conf";
    # extraConfig = builtins.readFile "${config.home.homeDirectory}/.tmux.conf";
  };
}
