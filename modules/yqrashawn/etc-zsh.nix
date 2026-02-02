{
  config,
  lib,
  pkgs,
  ...
}:

let
  paths = [
    "$HOME/Library/CloudStorage/Dropbox/sync/scripts"
    "$HOME/.local/bin"
    "$HOME/local/bin"
    "$HOME/.config/.foundry/bin"
    "$HOME/common-lisp/lem"
    "$HOME/.emacs.d/bin"
    "/Applications/kitty.app/Contents/MacOS"
    "/Applications/Camoufox.app/Contents/MacOS"
    "/Applications/Firefox Developer Edition.app/Contents/MacOS"
    "$HOME/.qlot/bin"
    "$HOME/.web3j"
    "$HOME/local/bin/funcs"
    "$HOME/.asdf/shims"
    "$HOME/.local/share/pnpm"
    # for usc https://github.com/software-mansion/universal-sierra-compiler
    "$HOME/.cargo/bin"
    "$HOME/.yarn/bin"
    "$HOME/.config/yarn/global/node_modules/.bin"
    "/etc/profiles/per-user/${config.user.name}/bin"
    "/run/current-system/sw/bin"
    # "/usr/local/opt/curl/bin"
    "/opt/homebrew/bin"
  ];
in
{
  programs.bash = {
    enable = true;
  };
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
      export NEXT_TURBOPACK_EXPERIMENTAL_USE_SYSTEM_TLS_CERTS=1
      export NODE_TLS_REJECT_UNAUTHORIZED=0
      export NODE_OPTIONS="--dns-result-order=ipv4first"
    '';
  };
}
