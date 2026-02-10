{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:

let
  secretsDir = "/Users/${config.user.name}/.openclaw-secrets";
in
{
  hm = {
    imports = [ inputs.nix-openclaw.homeManagerModules.openclaw ];

    programs.openclaw = {
      documents = ../openclaw-documents;

      instances.default = {
        enable = true;
        launchd.enable = true;
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
            enabled = true;
            tokenFile = "${secretsDir}/telegram-bot-token";
            allowFrom = [ 470507106 ];
            proxy = "http://127.0.0.1:6152";
            network.autoSelectFamily = false;
          };

          channels.whatsapp = {
            allowFrom = [ "+8613021234545" ];
          };

          tools = {
            alsoAllow = [ "lobster" "llm_task" "exec" "reactions" ];
            exec.applyPatch.enabled = true;
          };

          plugins.entries.telegram.enabled = true;
          plugins.entries.whatsapp.enabled = true;
        };
      };
    };

    home.file.".openclaw/openclaw.json" = {
      force = true;
    };

    # Surge network extension does MITM with a self-signed CA for ad blocking.
    # NODE_EXTRA_CA_CERTS tells Node.js (and undici) to trust it.
    # Proxy env vars route traffic through Surge's explicit proxy rather than
    # the transparent VIF intercept (which breaks undici's TLS handshake).
    launchd.agents."com.steipete.openclaw.gateway".config.EnvironmentVariables = {
      NODE_EXTRA_CA_CERTS = "/Users/${config.user.name}/Dropbox/sync/DlerCloud.pem";
      HTTP_PROXY = "http://127.0.0.1:6152";
      HTTPS_PROXY = "http://127.0.0.1:6152";
      ALL_PROXY = "socks5://127.0.0.1:6153";
      NODE_TLS_REJECT_UNAUTHORIZED = "0";
    };

    # Inject secrets into the launchd user domain so the openclaw service
    # can read them. launchctl setenv sets env vars that new launchd
    # services inherit.
    home.activation.openclawSecrets = inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      # Ensure nix-managed binaries (tailscale, etc.) are visible to launchd services
      /bin/launchctl setenv PATH "/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin:/usr/bin:/bin:/usr/sbin:/sbin"
      /bin/launchctl setenv NODE_EXTRA_CA_CERTS "/Users/${config.user.name}/Dropbox/sync/DlerCloud.pem"
      /bin/launchctl setenv NODE_TLS_REJECT_UNAUTHORIZED "0"
      /bin/launchctl setenv HTTP_PROXY "http://127.0.0.1:6152";
      /bin/launchctl setenv HTTPS_PROXY "http://127.0.0.1:6152";
      /bin/launchctl setenv ALL_PROXY "socks5://127.0.0.1:6153";
      /bin/launchctl setenv NODE_TLS_REJECT_UNAUTHORIZED "0";
      if [ -f "${secretsDir}/anthropic-oauth-token" ]; then
        ANTHROPIC_TOKEN="$(/bin/cat "${secretsDir}/anthropic-oauth-token")"
        /bin/launchctl setenv ANTHROPIC_OAUTH_TOKEN "$ANTHROPIC_TOKEN"
      fi
      if [ -f "${secretsDir}/gateway-token" ]; then
        GW_TOKEN="$(/bin/cat "${secretsDir}/gateway-token")"
        /bin/launchctl setenv OPENCLAW_GATEWAY_TOKEN "$GW_TOKEN"
        # Patch gateway.auth.token into openclaw.json so the CLI can authenticate
        OCJSON="$HOME/.openclaw/openclaw.json"
        if [ -f "$OCJSON" ]; then
          ${pkgs.jq}/bin/jq --arg tok "$GW_TOKEN" '.gateway.auth.token = $tok' "$OCJSON" > "$OCJSON.tmp" \
            && /bin/mv "$OCJSON.tmp" "$OCJSON"
        fi
      fi
    '';
  };
}
