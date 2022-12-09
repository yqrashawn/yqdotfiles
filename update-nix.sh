#!/usr/bin/env sh

# https://nixos.org/manual/nix/stable/installation/upgrading.html
sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && sleep 3 && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
