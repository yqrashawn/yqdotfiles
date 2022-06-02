{ config, inputs, pkgs, lib, ... }: {
  imports = [
    ./zsh.nix
    ./tmux.nix
    ./starship.nix
    ./fzf.nix
    ./direnv.nix
    ./bat.nix
    ./mcfly.nix
    ./zoxide.nix
    ./git.nix
    ./exa.nix
  ];
  home.packages = [ pkgs.tree ];
  programs = {
    jq.enable = true;
    htop.enable = true;
    gpg.enable = true;
    go.enable = true;
    nix-index.enable = true;
  };
}
