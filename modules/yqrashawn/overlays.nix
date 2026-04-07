{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  home = "${if pkgs.stdenvNoCC.isDarwin then "/Users" else "/home"}/${config.user.name}";
  overlays = final: prev: import ../../overlays/default.nix { inherit final prev pkgs; };
in
{
  nixpkgs.overlays = [
    overlays
    inputs.zed.overlays.default
    inputs.emacs-lsp-booster.overlays.default
    # inputs.nix-openclaw.overlays.default
    (
      final: prev:
      if prev.stdenv.hostPlatform.isAarch64 then inputs.clojure-lsp.overlays.default final prev else { }
    )
    inputs.emacs-overlay.overlays.default
    (final: prev: {
      emacs30-overlay =
        (prev.emacs-git.override {
          withNS = true;
          withSQLite3 = true;
          withWebP = true;
          withImageMagick = true;
          # withXwidgets = true;
          # withNativeCompilation = false;
          withNativeCompilation = true;
          withTreeSitter = true;
          withJansson = true;
          withMailutils = true;
          withCsrc = true;
        }).overrideAttrs
          (old: {
            name = "emacs30";
            version = "30.0-${inputs.emacs-custom-src.shortRev}";
            src = inputs.emacs-custom-src;
            patches = old.patches ++ [ ];
            buildInputs = old.buildInputs ++ [ ];
            configureFlags = old.configureFlags ++ [ "--with-xwidgets" ];
            # withMacport = true;
            # macportVersion = "master";
            # webkitgtk = true;
            # texinfo = true;
            # autoreconfHook = true;
          });
    })
    # rtk - CLI proxy that reduces LLM token consumption
    (final: prev: {
      rtk = prev.stdenv.mkDerivation rec {
        pname = "rtk";
        version = "0.34.3";
        src = prev.fetchurl {
          url = "https://github.com/rtk-ai/rtk/releases/download/v${version}/rtk-${
            if prev.stdenv.hostPlatform.isAarch64 then "aarch64" else "x86_64"
          }-apple-darwin.tar.gz";
          sha256 =
            if prev.stdenv.hostPlatform.isAarch64 then
              "sha256:945f644a77e5da3367142a999c41a4fa448d0a4ae3e61c8a45094b8522dba047"
            else
              "sha256:35928229a7fe064016b7cd567e9333278c661221e2a19180d4f1943516a8c1f1";
        };
        sourceRoot = ".";
        dontBuild = true;
        installPhase = ''
          install -Dm755 rtk $out/bin/rtk
        '';
        meta = with prev.lib; {
          description = "CLI proxy that reduces LLM token consumption by 60-90% on common dev commands";
          homepage = "https://github.com/rtk-ai/rtk";
          platforms = platforms.darwin;
        };
      };
    })
    # lightpanda - headless browser for AI agents
    (final: prev: {
      lightpanda = prev.stdenv.mkDerivation rec {
        pname = "lightpanda";
        version = "0.2.6";
        src = prev.fetchurl {
          url = "https://github.com/lightpanda-io/browser/releases/download/v${version}/lightpanda-${
            if prev.stdenv.hostPlatform.isAarch64 then "aarch64" else "x86_64"
          }-${if prev.stdenv.hostPlatform.isDarwin then "macos" else "linux"}";
          sha256 =
            if prev.stdenv.hostPlatform.isAarch64 then
              "sha256-6fdqFy/XAQi1sj8S0qC0LjfDbfaszOXoD8OVhc4/lcE="
            else
              "sha256-Nt7v+b3xJwnJDIJpdCLk/IPz33wtnkf4SyZChEbVvDY=";
        };
        dontUnpack = true;
        dontBuild = true;
        installPhase = ''
          install -Dm755 $src $out/bin/lightpanda
        '';
        meta = with prev.lib; {
          description = "Open-source headless browser for AI agents";
          homepage = "https://github.com/lightpanda-io/browser";
          platforms = [
            "aarch64-darwin"
            "x86_64-darwin"
          ];
        };
      };
    })
    # loki-mcp: MCP server for Loki
    (final: prev: {
      loki-mcp = prev.buildGoModule rec {
        pname = "loki-mcp";
        version = "0.6.0";
        src = prev.fetchFromGitHub {
          owner = "grafana";
          repo = "loki-mcp";
          rev = "v${version}";
          hash = "sha256-rRnKsE/jiOcvLZ8keN+i5X3nw8Hyx4wdq6w6EpAPjZ0=";
        };
        vendorHash = "sha256-xu9/BBKiUMNUhQQZCaifCoYtu0fkXqyx2aZUGQh2hGQ=";
        ldflags = [ "-s" "-w" ];
        doCheck = false;
      };
    })
    # mcp-prometheus: MCP server for Prometheus
    (final: prev: {
      mcp-prometheus = prev.buildGoModule rec {
        pname = "mcp-prometheus";
        version = "0.1.2";
        src = prev.fetchFromGitHub {
          owner = "giantswarm";
          repo = "mcp-prometheus";
          rev = "v${version}";
          hash = "sha256-FCplEr+6ZjdPzpVuiRSIq1HCEYMyuq6uImJBh1BhtA4=";
        };
        vendorHash = "sha256-jCW5cL/d1CJVIZE++YVtDACIemGeGQr37OJYhgMCXsA=";
        ldflags = [ "-s" "-w" ];
      };
    })
    # worktrunk
    (final: prev: {
      worktrunk = inputs.worktrunk.packages.${prev.system}.default;
    })
    # channels
    (final: prev: {
      # expose other channels via overlays
      stable = import inputs.stable { system = prev.system; };
      small = import inputs.small { system = prev.system; };
      darwins = import inputs.darwin-stable { system = prev.system; };
      masters = import inputs.nixpkgs-master { system = prev.system; };
      own = import inputs.own { system = prev.system; };
      # old = import inputs.old { system = prev.system; };
      # emacsMacport = import overlays.emacsMacport { system = prev.system; };
    })
  ];
}
