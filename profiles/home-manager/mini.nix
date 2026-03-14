{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./yqrashawn.nix ];

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks."*" = {
      extraOptions = {
        UseKeychain = "yes";
        AddKeysToAgent = "yes";
      };
    };
  };
}
