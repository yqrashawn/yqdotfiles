{ config, pkgs, ... }: {
  programs.kitty = {
    enable = true;
    package = pkgs.kitty;
    font = {
      name = "PragmataPro Mono Liga Regular";
      size = 18;
    };
    settings = {
      bold_font = "PragmataPro Mono Liga Bold";
      italic_font = "PragmataPro Mono Liga Italic";
      bold_italic_font = "PragmataPro Mono Liga Bold Italic";
      # font_size = (if pkgs.stdenvNoCC.isDarwin then 18 else 18);
      term = "xterm-256color";
      scrollback_lines = 10000;
      copy_on_select = "yes";
      strip_trailing_spaces = "smart";
      sync_to_monitor = "no";
      enable_audio_bell = "no";
      visual_bell_duration = "0.0";
      window_alert_on_bell = "no";
      tab_bar_edge = "bottom";
      macos_titlebar_color = "background";
      macos_option_as_alt = "yes";
      macos_traditional_fullscreen = "yes";
      macos_show_window_title_in = "window";
      shell_integration = "no-cursor";
    };
    extraConfig = ''
      include current-theme.conf
    '';
  };
}
