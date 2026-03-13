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
    };
  };
}
