{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
    # enableFzfCompletion = true;
    # enableSyntaxHighlighting = true;
    # enableFzfHistory = true;
    # enableFzfGit = true;
    interactiveShellInit = ''
      export PATH="$HOME/.fnm/current/bin:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Sublime Text.app/Contents/SharedSupport/bin:$HOME/.emacs.d/bin:$HOME/local/bin/funcs:$HOME/local/bin:$PATH"
    '';
  };
}
