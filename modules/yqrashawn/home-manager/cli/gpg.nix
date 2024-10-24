{ config, lib, pkgs, ... }:

{
  programs.gpg = {
    enable = true;
    # use gnupg 2.4.0
    # until this is fixed https://dev.gnupg.org/T6481
    # package = pkgs.darwins.gnupg;
    settings = { pinentry-mode = "loopback"; };
  };

  # services.gpg-agent = {
  #   enable = true;
  #   defaultCacheTtl = 1800;
  #   maxCacheTtl = 7200;
  #   enableSshSupport = true;
  #   # use auth subkey's keygrip: gpg2 -K --with-keygrip
  #   sshKeys = [ "B198FB15EC5C13012E940B37E394C5D9A8E535A6" ];
  #   enableBashIntegration = true;
  #   enableZshIntegration = true;
  #   extraConfig = ''
  #     allow-emacs-pinentry
  #     allow-loopback-pinentry
  #     log-file /var/log/gpg-agent.log
  #     use-standard-socket
  #   '';
  # };
}
