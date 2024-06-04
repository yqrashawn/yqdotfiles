{ config, lib, pkgs, ... }:

let
  paths = [
    "$HOME/Library/CloudStorage/Dropbox/sync/scripts"
    "$HOME/common-lisp/lem"
    "/Applications/Firefox Developer Edition.app/Contents/MacOS"
    "$HOME/.qlot/bin"
    "$HOME/.web3j"
    "$HOME/.local/share/pnpm"
    "$HOME/.config/.foundry/bin"
    "$HOME/.foundry/bin"
    "/Applications/Emacs.app/Contents/MacOS/bin"
    "$HOME/.emacs.d/bin"
    "$HOME/local/bin/funcs"
    "$HOME/local/bin"
    "$HOME/.cargo/bin"
    "$HOME/.yarn/bin"
    "$HOME/.config/yarn/global/node_modules/.bin"
    "/run/current-system/sw/bin"
    # "/usr/local/opt/curl/bin"
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
