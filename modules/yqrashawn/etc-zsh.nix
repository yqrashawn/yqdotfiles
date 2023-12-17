{ config, lib, pkgs, ... }:

let
  paths = [
    "$HOME/common-lisp/lem"
    "$HOME/.qlot/bin"
    "$HOME/.web3j"
    "$HOME/.config/.foundry/bin"
    "$HOME/.foundry/bin"
    "/Applications/Emacs.app/Contents/MacOS/bin"
    "$HOME/.emacs.d/bin"
    "$HOME/local/bin/funcs"
    "$HOME/local/bin"
    "$HOME/.cargo/bin"
    "$HOME/.yarn/bin"
    "$HOME/.config/yarn/global/node_modules/.bin"
    "/usr/local/opt/curl/bin"
    "/opt/homebrew/bin"
  ];
in {
  programs.bash = { enable = true; };
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
    # enableFzfCompletion = true;
    # enableSyntaxHighlighting = true;
    # enableFzfHistory = true;
    # enableFzfGit = true;
    interactiveShellInit = ''
      export PATH="${builtins.concatStringsSep ":" paths}:$PATH"
    '';
  };
}
