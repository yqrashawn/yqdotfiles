{ config, lib, pkgs, inputs, ... }: {
  home.file.".tmux.conf" = { source = "${inputs.tmux-conf}/.tmux.conf"; };
  home.file.".tmux.conf.local" = { source = ./.tmux.conf.local; };
  # programs.tmux = {enable = true;};
}
