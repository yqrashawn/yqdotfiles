{ config, lib, pkgs, ... }:

let
  useSkim = false;
  useFzf = !useSkim;
  fuzz = let fd = "${pkgs.fd}/bin/fd";
  in rec {
    defaultCommand = "${fd} -H --type f";
    defaultOptions = [ "--height 50%" ];
    fileWidgetCommand = "${defaultCommand}";
    fileWidgetOptions = [
      "--preview '${pkgs.bat}/bin/bat --color=always --plain --line-range=:200 {}'"
    ];
    changeDirWidgetCommand = "${fd} --type d";
    changeDirWidgetOptions =
      [ "--preview '${pkgs.tree}/bin/tree -C {} | head -200'" ];
    # historyWidgetOptions = [ "--tac" "--exact" ];
    historyWidgetOptions = [ ];
  };
in {
  programs.skim = {
    enable = useSkim;
    enableBashIntegration = useSkim;
    enableZshIntegration = useSkim;
    enableFishIntegration = useSkim;
  } // fuzz;

  programs.fzf = {
    enable = useFzf;
    enableBashIntegration = useFzf;
    # enableZshIntegration = useFzf;
    enableZshIntegration = false;
    enableFishIntegration = useFzf;
  } // fuzz;
}
