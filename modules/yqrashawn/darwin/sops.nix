{
  config,
  pkgs,
  ...
}:
let
  username = config.user.name;
in
{
  environment.systemPackages = with pkgs; [ age ];

  sops = {
    defaultSopsFile = ../../../secrets/secrets.yaml;
    age.keyFile = "/Users/${username}/.config/sops/age/keys.txt";

    secrets = {
      ssh-config = {
        sopsFile = ../../../secrets/ssh-config.yaml;
        path = "/Users/${username}/.ssh/config";
        owner = username;
        mode = "0600";
      };

      mbsyncrc = {
        sopsFile = ../../../secrets/mbsyncrc.yaml;
        path = "/Users/${username}/.mbsyncrc";
        owner = username;
        mode = "0600";
      };

      msmtprc = {
        sopsFile = ../../../secrets/msmtprc.yaml;
        path = "/Users/${username}/.msmtprc";
        owner = username;
        mode = "0600";
      };

      notmuch-config = {
        sopsFile = ../../../secrets/notmuch-config.yaml;
        path = "/Users/${username}/.notmuch-config";
        owner = username;
        mode = "0600";
      };

      ntf = {
        sopsFile = ../../../secrets/ntf.yaml;
        path = "/Users/${username}/.ntf.yml";
        owner = username;
        mode = "0600";
      };

      atuin = {
        sopsFile = ../../../secrets/atuin.yaml;
        path = "/Users/${username}/.config/atuin/config.toml";
        owner = username;
        mode = "0600";
      };

      atuin-server = {
        sopsFile = ../../../secrets/atuin-server.yaml;
        path = "/Users/${username}/.config/atuin/server.toml";
        owner = username;
        mode = "0600";
      };

      gitleaks = {
        sopsFile = ../../../secrets/gitleaks.yaml;
        path = "/Users/${username}/.config/.gitleaks.toml";
        owner = username;
        mode = "0600";
      };
    };
  };
}
