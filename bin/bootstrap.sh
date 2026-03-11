#!/usr/bin/env sh

# Use short hostname as config target (matches darwinConfigurations keys)
HOST="$(hostname -s)"
nix --extra-experimental-features "nix-command flakes" develop -c sysdo bootstrap --darwin "$HOST"
# nix --extra-experimental-features "nix-command flakes" develop -c sysdo bootstrap --home-manager "$HOST"
