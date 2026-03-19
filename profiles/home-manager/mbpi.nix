{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./yqrashawn.nix ];

  programs.git.signing.signByDefault = lib.mkForce false;
}
