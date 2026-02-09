{ config, inputs, pkgs, lib, ... }:

let
  secretsDir = "/Users/${config.user.name}/.openclaw-secrets";
in {
  hm = {
    imports = [ inputs.nix-openclaw.homeManagerModules.openclaw ];

    programs.openclaw = {
      documents = ../openclaw-documents;

      instances.default = {
        enable = true;
        launchd.enable = true;
      };

      config = {
        gateway = {
          mode = "local";
          tailscale = {
            mode = "serve";
            resetOnExit = true;
          };
        };

        agents.defaults.model = {
          primary = "anthropic/claude-opus-4-6";
        };

        channels.telegram = {
          tokenFile = "${secretsDir}/telegram-bot-token";
          allowFrom = [ 470507106 ];
        };
      };
    };

    # Inject secrets into the launchd user domain so the openclaw service
    # can read them. launchctl setenv sets env vars that new launchd
    # services inherit.
    home.activation.openclawSecrets =
      inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ -f "${secretsDir}/anthropic-oauth-token" ]; then
          ANTHROPIC_TOKEN="$(/bin/cat "${secretsDir}/anthropic-oauth-token")"
          /bin/launchctl setenv ANTHROPIC_OAUTH_TOKEN "$ANTHROPIC_TOKEN"
        fi
        if [ -f "${secretsDir}/gateway-token" ]; then
          GW_TOKEN="$(/bin/cat "${secretsDir}/gateway-token")"
          /bin/launchctl setenv OPENCLAW_GATEWAY_TOKEN "$GW_TOKEN"
        fi
      '';
  };
}
