#!/usr/bin/env sh

# https://nixos.org/manual/nix/stable/installation/upgrading.html
sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix'
sudo launchctl remove org.nixos.nix-daemon
sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
