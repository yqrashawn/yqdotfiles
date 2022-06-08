{ config, lib, pkgs, inputs, ... }: {
  home.file.".tmux.conf" = { source = "${inputs.tmux-conf}/.tmux.conf"; };
  home.file.".tmux.conf.local" = { source = ./.tmux.conf.local; };
  programs.tmux = {
    enable = true;
    # extraConfig = "source ~/.tmux/.tmux.conf";
    # extraConfig = builtins.readFile "${config.home.homeDirectory}/.tmux.conf";
  };
}
