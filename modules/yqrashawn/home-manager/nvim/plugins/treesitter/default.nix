{
  config,
  pkgs,
  lib,
  ...
}:
{
  home.packages = [ pkgs.tree-sitter ];
  programs.neovim = {
    plugins = with pkgs.vimPlugins; [
      # new neovim stuff
      (config.lib.vimUtils.pluginWithCfg {
        plugin = (nvim-treesitter.withPlugins (_: nvim-treesitter.allGrammars));
        file = ./nvim-treesitter.lua;
      })
      (config.lib.vimUtils.pluginWithCfg {
        plugin = pkgs.vimUtils.buildVimPlugin {
          pname = "nvim-treesitter-textobjects";
          version = "2026-03-08";
          src = pkgs.fetchFromGitHub {
            owner = "nvim-treesitter";
            repo = "nvim-treesitter-textobjects";
            rev = "4e91b5d0394329a229725b021a8ea217099826ef";
            hash = "sha256-n+GX7w+W3D3xj2+3Zr+89E7RUqBgWU14BlQrGXztoH0=";
          };
          doCheck = false;
        };
        file = ./nvim-treesitter-textobjects.lua;
      })
    ];
  };
}
