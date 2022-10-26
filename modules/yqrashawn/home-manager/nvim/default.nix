{ config, pkgs, lib, ... }: {
  imports = [ ./plugins ];

  lib.vimUtils = rec {
    # For plugins configured with lua
    wrapLuaConfig = luaConfig: ''
      lua<<EOF
      ${luaConfig}
      EOF
    '';
    readVimConfig = file:
      if (lib.strings.hasSuffix ".lua" (builtins.toString file)) then
        wrapLuaConfig (builtins.readFile file)
      else
        builtins.readFile file;
    pluginWithCfg = { plugin, file }: {
      inherit plugin;
      config = readVimConfig file;
    };
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    # nvim plugin providers
    withNodeJs = true;
    withRuby = true;
    withPython3 = true;

    # share vim plugins since nothing is specific to nvim
    plugins = with pkgs.vimPlugins; [
      # basics
      vim-sensible
      vim-sandwich
      vim-commentary
      vim-nix
      vim-which-key

      # Text objects
      nvim-autopairs
      nvim-comment
      nvim-surround

      # Appearance
      bufferline-nvim
      indent-blankline-nvim
      lualine-nvim
      nvim-colorizer-lua

      # Fuzzy Finder
      cheatsheet-nvim
      telescope-fzf-native-nvim
      telescope-nvim

      # General Deps
      nui-nvim
      plenary-nvim
      popup-nvim

      # Project management
      direnv-vim
      project-nvim

      # Git
      gitsigns-nvim
      vim-fugitive

      # vim addon utilities
      direnv-vim
      ranger-vim
      neogit
    ];
    extraPackages = with pkgs; [ tree-sitter ];
    extraConfig = ''
      ${config.lib.vimUtils.readVimConfig ./settings.lua}
    '';
  };

}
