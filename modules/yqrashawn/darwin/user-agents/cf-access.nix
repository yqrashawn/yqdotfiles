{ config, lib, pkgs, ... }:

let script = "${config.user.home}/Dropbox/sync/scripts/cf_access.sh";
in {
  launchd.user.agents.cf-access = {
    command = "sh ${script}";
    serviceConfig = {
      RunAtLoad = true;
      KeepAlive = true;
      StandardErrorPath = "/tmp/cf-access-stderr.log";
      StandardOutPath = "/tmp/cf-access-stdout.log";
    };
  };
}
