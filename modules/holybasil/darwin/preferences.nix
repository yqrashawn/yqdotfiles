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
      NSWindowResizeTime = "0.001";

      InitialKeyRepeat = 10;
      KeyRepeat = 1;

      PMPrintingExpandedStateForPrint = true;
      PMPrintingExpandedStateForPrint2 = true;

      # Use F1, F2, etc. keys as standard function keys
      "com.apple.keyboard.fnState" = false;

      # tap to click
      "com.apple.mouse.tapBehavior" = 1;

      # Sets the beep/alert volume level from 0.000 (muted) to 1.000 (100% volume).
      "com.apple.sound.beep.volume" = "0.6065307";

      # Make a feedback sound when the system volume changed. This setting accepts the integers 0 or 1. Defaults to 1
      "com.apple.sound.beep.feedback" = 1;

      # Whether to enable trackpad secondary click.  The default is true
      "com.apple.trackpad.enableSecondaryClick" = true;

      "com.apple.trackpad.trackpadCornerClickBehavior" = null;

      # trackpad tracking speed 0-3
      "com.apple.trackpad.scaling" = "2.8";

      # Enable spring loading for directories
      "com.apple.springing.enabled" = true;

      # Remove the spring loading delay for directories
      "com.apple.springing.delay" = "0";
      "com.apple.swipescrolldirection" = true;

      AppleMeasurementUnits = "Centimeters";
      # Whether to use the metric system.  The default is based on region settings
      AppleMetricUnits = null;
      AppleTemperatureUnit = "Celsius";

      # Whether to autohide the menu bar.  The default is false
      _HIHideMenuBar = false;
    };

    SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

    universalaccess = {
      reduceTransparency = true;
      # Use scroll gesture with the Ctrl (^) modifier key to zoom
      closeViewScrollWheelToggle = true;
      # zoom view focus follow keyboard
      closeViewZoomFollowsFocus = true;
    };

    # login window settings
    loginwindow = {
      # Displays login window as a name and password field instead of a list of users
      SHOWFULLNAME = false;

      autoLoginUser = null;

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
      autohide-delay = "0.0";
      # how fast is the dock showing animation
      autohide-time-modifier = "0.0";
      dashboard-in-overlay = false;
      enable-spring-load-actions-on-all-items = true;
      expose-animation-duration = "0.01";
      expose-group-by-app = false;
      # app launch animation
      launchanim = false;
      mineffect = null;
      minimize-to-application = false;
      # Enable highlight hover effect for the grid view of a stack (Dock)
      mouse-over-hilite-stack = true;
      # Whether to automatically rearrange spaces based on most recent use.  The default is true.
      mru-spaces = false;
      orientation = "right";
      show-process-indicators = true;
      showhidden = true;
      show-recents = true;
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
}
