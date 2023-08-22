{ config, lib, pkgs, ... }:

let uhome = config.home.homeDirectory;
in {
  programs.zsh.prezto = {
    enable = true;
    caseSensitive = false;
    color = true;
    # pmoduleDirs = [ "$HOME/.zprezto-contrib" ];
    extraModules = [ "attr" "stat" ];
    extraFunctions = [ "zargs" "zmv" ];
    pmodules = [
      "environment"
      "tmux"
      "terminal"
      "editor"
      # "history"
      "directory"
      "utility"
      "archive"
      "docker"
      "homebrew"
      "osx"
      "node"
      "ssh"
      "git"
      "spectrum"
      # "python"
      "command-not-found"
      # "ruby"
      # "completion"
      # "autosuggestions"
      # "history-substring-search"
    ];
    editor = {
      keymap = "emacs";
      dotExpansion = true;
      promptContext = true;
    };
    gnuUtility.prefix = "g";
    macOS.dashKeyword = "mand";
    python = {
      virtualenvAutoSwitch = true;
      virtualenvInitialize = true;
    };
    terminal = {
      autoTitle = true;
      windowTitleFormat = "%n@%m: %s %d";
      tabTitleFormat = "%m: %s %d";
      multiplexerTitleFormat = "%n@%m: %s %d";
    };
    tmux = { autoStartLocal = true; };
    ssh.identities = [ "id_rsa" "id_rsa_holy" "id_rsa_website_jump" ];
  };
}
