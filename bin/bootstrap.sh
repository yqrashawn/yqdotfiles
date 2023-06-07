#!/usr/bin/env sh

# yqrashawn is the bootstrap target
nix --extra-experimental-features "nix-command flakes" develop -c sysdo bootstrap --darwin yqrashawn
nix --extra-experimental-features "nix-command flakes" develop -c sysdo bootstrap --home-manager yqrashawn
