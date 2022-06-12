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
      export PATH="/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Sublime Text.app/Contents/SharedSupport/bin:$HOME/.emacs.d/bin:$HOME/local/bin/funcs:$HOME/local/bin:$HOME/.cargo/bin:/usr/local/opt/curl/bin:/opt/homebrew/bin:$PATH"
    '';
  };
}
