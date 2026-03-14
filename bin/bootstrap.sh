#!/usr/bin/env bash

set -euo pipefail

nix --extra-experimental-features "nix-command flakes" develop -c sysdo bootstrap --darwin "$HOST"
# nix --extra-experimental-features "nix-command flakes" develop -c sysdo bootstrap --home-manager "$HOST"
