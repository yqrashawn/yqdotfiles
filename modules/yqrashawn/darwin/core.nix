{ inputs, config, pkgs, ... }:
let
  home = builtins.getEnv "HOME";
  prefix = "/run/current-system/sw/bin";
  tmpdir = "/tmp";
  localconfig = import <localconfig>;
  xdg_configHome = "${home}/.config";
  xdg_dataHome = "${home}/.local/share";
  xdg_cacheHome = "${home}/.cache";
  iterate = StartInterval: {
    inherit StartInterval;
    Nice = 5;
    LowPriorityIO = true;
    AbandonProcessGroup = true;
  };
  runCommand = command: {
    inherit command;
    serviceConfig.RunAtLoad = true;
    serviceConfig.KeepAlive = true;
  };
in {
  imports = [
    ./daemons
    ./user-agents # ./network.nix
  ];

  security.pki.certificateFiles = [
    "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    "/etc/ssl/cert.pem"
    # "${config.user.home}/Dropbox/sync/cert.pem"
  ];

  # environment setup
  environment = {
    pathsToLink = [
      "/share/zsh"
      "/share/hunspell"
      "/share/aspell"
      # "/share/postgresql"
    ];
    # loginShell = pkgs.zsh;
    # backupFileExtension = "backup";
    etc = {
      darwin.source = "${inputs.darwin}";
      profile = { source = ./root-profile; };
    };
    variables = {
      EDITOR = "emacsclient";
      LSP_USE_PLISTS = "true";
      # HTTPS_PROXY = "http://127.0.0.1:6152";
      # HTTP_PROXY = "http://127.0.0.1:6152";
      # ALL_PROXY = "socks5://127.0.0.1:6153";
    };

    # launchDaemons = {
    #   "limit.maxfile.plist" = { source = ./limit.maxfile.plist; };
    #   "limit.maxproc.plist" = { source = ./limit.maxproc.plist; };
    #   "set-nix-path.plist" = { source = ./set-nix-path.plist; };
    # };

    # Use a custom configuration.nix location.
    # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix

    # packages installed in system profile
    systemPackages = with pkgs; [ nix-doc sops ];
  };

  nix.enable = true;
  nix.nixPath = [ "darwin=/etc/${config.environment.etc.darwin.target}" ];
  nix.extraOptions = ''
    extra-platforms = x86_64-darwin aarch64-darwin
  '';

  # auto manage nixbld users with nix darwin
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.info.enable = true;
  programs.man.enable = true;

  # Auto upgrade nix package and the daemon service.
  services.lorri.enable = true;
  # services.yabai = {
  #   enable = true;
  #   enableScriptingAddition = true;
  # };
  # services.emacs.package = pkgs.emacsMacport;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system = {
    primaryUser = "yqrashawn";
    # stateVersion = 4;
    defaults = {
      NSGlobalDomain = {
        ApplePressAndHoldEnabled = false;
        AppleKeyboardUIMode = 3; # full keyboard control
        _HIHideMenuBar = false; # auto hide menubar
      };

      ".GlobalPreferences" = {
        "com.apple.sound.beep.sound" = "/System/Library/Sounds/Funk.aiff";
      };

      dock = {
        autohide = true;
        launchanim = false;
        orientation = "bottom";
      };
      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = true;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  documentation = {
    enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  };
}
