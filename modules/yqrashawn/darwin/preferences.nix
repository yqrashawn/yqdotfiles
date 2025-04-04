{ config, pkgs, ... }:
let home = builtins.getEnv "HOME";
in {
  system.defaults = {
    LaunchServices.LSQuarantine = false;

    NSGlobalDomain = {
      AppleShowAllFiles = true;
      # magic mouse swipe nav
      AppleEnableMouseSwipeNavigateWithScrolls = true;
      # trackpad swipe nav
      AppleEnableSwipeNavigateWithScrolls = true;

      # Enable subpixel font rendering on non-Apple LCDs
      # Reference: https://github.com/kevinSuttle/macOS-Defaults/issues/17#issuecomment-266633501
      AppleFontSmoothing = 1;

      # dark light auto
      AppleInterfaceStyle = null;
      AppleInterfaceStyleSwitchesAutomatically = true;

      # full keyboard control
      AppleKeyboardUIMode = 3;

      # allow key repeat
      ApplePressAndHoldEnabled = false;

      AppleShowAllExtensions = true;
      AppleShowScrollBars = "Automatic";

      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      # window open/close animation
      NSAutomaticWindowAnimationsEnabled = false;
      # disable auto terminalion of inactive app
      NSDisableAutomaticTermination = false;
      # save doc to icloud
      NSDocumentSaveNewDocumentsToCloud = false;
      # expand save panel by default
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
      # sidebar icon size 1 small, 2 mdeium, 3 large, default is 3
      NSTableViewDefaultSizeMode = 2;
      # Whether to display ASCII control characters using caret notation in standard text views. The default is false.
      NSTextShowsControlCharacters = true;

      # Whether to enable the focus ring animation. The default is true.
      NSUseAnimatedFocusRing = false;

      # Whether to enable smooth scrolling. The default is true.
      NSScrollAnimationEnabled = true;

      # Sets the speed speed of window resizing. The default is given in the example.
      NSWindowResizeTime = 1.0e-3;

      InitialKeyRepeat = 10;
      KeyRepeat = 1;

      PMPrintingExpandedStateForPrint = true;
      PMPrintingExpandedStateForPrint2 = true;

      # Use F1, F2, etc. keys as standard function keys
      "com.apple.keyboard.fnState" = false;

      # tap to click
      "com.apple.mouse.tapBehavior" = 1;

      # Sets the beep/alert volume level from 0.000 (muted) to 1.000 (100% volume).
      "com.apple.sound.beep.volume" = 0.606531;

      # Make a feedback sound when the system volume changed. This setting accepts the integers 0 or 1. Defaults to 1
      "com.apple.sound.beep.feedback" = 1;

      # Whether to enable trackpad secondary click.  The default is true
      "com.apple.trackpad.enableSecondaryClick" = true;

      "com.apple.trackpad.trackpadCornerClickBehavior" = null;

      # trackpad tracking speed 0-3
      "com.apple.trackpad.scaling" = 2.8;

      # Enable spring loading for directories
      "com.apple.springing.enabled" = true;

      # Remove the spring loading delay for directories
      "com.apple.springing.delay" = 0.0;
      "com.apple.swipescrolldirection" = true;

      AppleMeasurementUnits = "Centimeters";
      # Whether to use the metric system.  The default is based on region settings
      AppleMetricUnits = null;
      AppleTemperatureUnit = "Celsius";

      # Whether to autohide the menu bar.  The default is false
      _HIHideMenuBar = false;
    };

    SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

    # universalaccess = {
    #   reduceTransparency = true;
    #   # Use scroll gesture with the Ctrl (^) modifier key to zoom
    #   closeViewScrollWheelToggle = true;
    #   # zoom view focus follow keyboard
    #   closeViewZoomFollowsFocus = true;
    # };

    # login window settings
    loginwindow = {
      # Displays login window as a name and password field instead of a list of users
      SHOWFULLNAME = false;

      autoLoginUser = config.user.name;

      # disable guest account
      GuestEnabled = false;

      LoginwindowText = "Welcome";

      ShutDownDisabled = false;
      RestartDisabled = false;
      ShutDownDisabledWhileLoggedIn = false;
      PowerOffDisabledWhileLoggedIn = false;
      RestartDisabledWhileLoggedIn = false;
      DisableConsoleAccess = false;
    };

    # file viewer settings
    finder = {
      AppleShowAllFiles = true;
      ShowStatusBar = true;
      ShowPathbar = true;
      FXDefaultSearchScope = "SCcf";
      FXPreferredViewStyle = "Nlsv";
      AppleShowAllExtensions = true;
      CreateDesktop = false;
      QuitMenuItem = true;
      _FXShowPosixPathInTitle = true;
      FXEnableExtensionChangeWarning = false;
    };

    # trackpad settings
    trackpad = {
      # enable tap to click
      Clicking = true;
      # Whether to enable tap-to-drag. The default is false.
      Dragging = false;
      # Whether to enable trackpad right click.  The default is false.
      TrackpadRightClick = true;
      TrackpadThreeFingerDrag = true;

      # silent clicking = 0, default = 1
      ActuationStrength = 0;

      # firmness 0 for light clicking, 1 for medium, 2 for firm.
      FirstClickThreshold = 1;
      # firmness level for force touch
      SecondClickThreshold = 1;
    };

    # firewall settings
    alf = {
      # 0 = disabled 1 = enabled 2 = blocks all connections except for essential services
      globalstate = 0;
    };

    # dock settings
    dock = {
      # auto show and hide dock
      autohide = true;
      # remove delay for showing dock
      autohide-delay = 0.0;
      # how fast is the dock showing animation
      autohide-time-modifier = 0.0;
      dashboard-in-overlay = false;
      enable-spring-load-actions-on-all-items = true;
      expose-animation-duration = 1.0e-2;
      expose-group-apps = false;
      # expose-group-by-app = false;
      # app launch animation
      launchanim = false;
      mineffect = null;
      minimize-to-application = false;
      # Enable highlight hover effect for the grid view of a stack (Dock)
      mouse-over-hilite-stack = true;
      # Whether to automatically rearrange spaces based on most recent use.  The default is true.
      mru-spaces = false;
      orientation = "bottom";
      show-process-indicators = true;
      showhidden = true;
      show-recents = false;
      static-only = false;
      tilesize = 50;
    };

    magicmouse.MouseButtonMode = "TwoButton";

    screencapture = {
      location = "${home}/Downloads";
      disable-shadow = false;
    };

    smb = {
      NetBIOSName = null;
      ServerDescription = null;
    };

    spaces.spans-displays = false;

  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  system.activationScripts = {
    extraUserActivation = {
      text = ''
        cuser="$(id -un)"

        rm -rf ~/.doom.d || true
        ln -s ~/.nixpkgs/.doom.d ~/.doom.d

        if [ ! -e ~/.local/share/yarn/global/package.json ]; then
            mkdir -p ~/.local/share/yarn/global
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yarn-package.json ~/.local/share/yarn/global/package.json
        fi

        if [ ! -e ~/.local/share/pnpm/global/5/package.json ]; then
            mkdir -p ~/.local/share/pnpm/global/5
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/pnpm-package.json ~/.local/share/pnpm/global/5/package.json
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/pnpm-lock.yaml ~/.local/share/pnpm/global/5/pnpm-lock.yaml
        fi

        rm -rf ~/.config/karabiner.edn || true

        if [ "$cuser" = "holybasil" ]; then
          ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/hkarabiner.edn ~/.config/karabiner.edn
        else
          ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/karabiner.edn ~/.config/karabiner.edn
        fi

        rm -rf ~/.config/yabai || true
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai ~/.config/yabai

        rm -rf ~/Dropbox || true
        ln -s ~/Library/CloudStorage/Dropbox ~/Dropbox

        rm -rf ~/.tridactylrc || true
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.tridactylrc ~/.tridactylrc

        # rm -rf ~/.tridactyl || true
        # ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/tridactyl ~/.tridactyl

        if [ "$cuser" = "yqrashawn" ]; then
          rm -rf ~/.authinfo.gpg || true
          ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.authinfo.gpg ~/.authinfo.gpg
        fi

        rm -rf ~/.spacehammer || true
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.spacehammer ~/.spacehammer

        rm -rf ~/.gitignore_global || true
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/gitignore_global ~/.gitignore_global

        if [ "$cuser" = "yqrashawn" ]; then
          rm ~/.ssh/config || true
          ${pkgs.gnupg}/bin/gpg --decrypt --output  ~/.ssh/config ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/ssh.gpg || true

          rm ~/.mbsyncrc || true
          ${pkgs.gnupg}/bin/gpg --decrypt --output  ~/.mbsyncrc ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.mbsyncrc.gpg || true
        fi

        ln -fs /Applications/Nix\ Apps/* /Applications/

        if [ -e ~/Dropbox/sync/oauth2token ] && [ ! -e ~/.local/share/oauth2token ]; then
            echo 'link oauth2token, pip install oauth2token'
            ln -s ~/Dropbox/sync/oauth2token ~/.local/share/oauth2token
        fi
        if [ -e ~/Dropbox/sync/ntf ] && [ ! -e /opt/homebrew/bin/ntf ]; then
            echo 'link /opt/homebrew/bin/ntf'
            ln -s ~/Dropbox/sync/ntf /opt/homebrew/bin/ntf
        fi
        if [ -e ~/Dropbox/sync/.ntf.yml ] && [ ! -e ~/.ntf.yml ]; then
            echo 'link ~/.ntf.yml'
            ln -s ~/Dropbox/sync/.ntf.yml ~/.ntf.yml
        fi
        if [ -e ~/Dropbox/sync/.notmuch-config ] && [ ! -e ~/.notmuch-config ]; then
            echo 'link .notmuch-config'
            ln -s ~/Dropbox/sync/.notmuch-config ~/.notmuch-config
        fi
        if [ -e ~/Dropbox/sync/.msmtprc ] && [ ! -e ~/.msmtprc ]; then
            echo 'link .msmtprc'
            ln -s ~/Dropbox/sync/.msmtprc ~/.msmtprc
        fi
        if [ -e ~/Dropbox/sync/personal_dictionaries/en_US.dic ] && [ ! -e ~/.config/enchant/en_US.dic ]; then
            echo 'link enchant/hunspell dictionaries'
            ln -s ~/Dropbox/sync/personal_dictionaries ~/.config/enchant
        fi
        if [ ! -e ~/.config/aerospace/aerospace.toml ]; then
            echo 'link aerospace.toml'
            mkdir -p ~/.config/aerospace/
            ln -s ~/.nixpkgs/modules/yqrashawn//home-manager/dotfiles/aerospace.toml ~/.config/aerospace/
        fi
        if [ ! -e ~/.config/clj-kondo ]; then
            echo 'link clj-kondo/config.edn'
            mkdir -p ~/.config
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/clj-kondo ~/.config/clj-kondo
        fi
        if [ ! -e ~/.tool-versions ]; then
            echo 'link ~/.tool-versions'
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.tool-versions ~/.tool-versions
        fi
        if [ ! -e ~/.zprintrc ]; then
            echo 'link ~/.zprintrc'
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.zprintrc ~/.zprintrc
        fi
        if [ ! -e ~/.config/zed ]; then
            echo 'link ~/.config/zed'
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/zed ~/.config/zed
        fi
        if [ ! -e ~/.config/kitty/kitty.conf ]; then
            echo 'link ~/.config/kitty/kitty.conf'
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/kitty.conf ~/.config/kitty/kitty.conf
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/kitty-map.conf ~/.config/kitty/kitty-map.conf
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/macos-launch-services-cmdline ~/.config/kitty/macos-launch-services-cmdline
        fi
        if [ ! -e ~/.config/nyxt/config.lisp ]; then
            echo 'link ~/.config/nyxt/config.lisp'
            mkdir -p ~/.config/nyxt
            ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/nyxt.lisp ~/.config/nyxt/config.lisp
        fi
      '';
    };
  };
}
