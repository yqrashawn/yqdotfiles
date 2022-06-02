{ config, lib, pkgs, ... }:

let home = config.home.homeDirectory;
in {
  programs.direnv = {
    enable = true;
    config = {
      global = { load_dotenv = true; };
      whitelist = {
        prefix = [ "${home}/workspace/office" "${home}/workspace/home" ];
      };
    };
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
    stdlib = ''
      # stolen from @i077; store .direnv in cache instead of project dir
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
          echo "''${direnv_layout_dirs[$PWD]:=$(
              echo -n "${config.xdg.cacheHome}"/direnv/layouts/
              echo -n "$PWD" | shasum | cut -d ' ' -f 1
          )}"
      }
    '';
  };
}
