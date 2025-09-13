# CLAUDE.nixpkgs.md

This file provides guidance to LLM when working with code in this repository.

## Project Overview

This is a personal dotfiles repository built with Nix flakes. It manages system configurations for macOS (nix-darwin) and user environments (home-manager) with a modular structure centered around the user "yqrashawn".

## Key Commands

### Build and Deploy
- `python3 bin/do.py build [configuration]` - Build specific flake configuration (auto-detects platform)
- `python3 bin/do.py switch [configuration]` - Build and activate configuration
- `python3 bin/do.py bootstrap` - Build initial configuration
- `sysdo` - Shorthand command (available after home-manager activation) that runs do.py from correct directory

### Development Utilities
- `python3 bin/do.py fmt` - Format all code files (nix, python, lua) using treefmt
- `python3 bin/do.py clean` - Remove previous builds and symlinks
- `python3 bin/do.py gc` - Run nix garbage collection
- `python3 bin/do.py update [input]` - Update flake inputs
- `python3 bin/do.py cache` - Cache flake output environment

### Git Operations
- `python3 bin/do.py git-pull` - Pull remote changes
- `python3 bin/do.py git-push` - Push local changes

## Architecture

### Flake Structure
- **flake.nix**: Main entry point defining inputs, outputs, and configurations
- **profiles/**: User-specific profiles (yqrashawn.nix, work.nix, holybasil.nix)
- **modules/yqrashawn/**: Core user modules split into:
  - **darwin/**: macOS-specific configurations (apps, daemons, preferences)
  - **home-manager/**: Cross-platform user environment (cli tools, dotfiles, git, nvim)
  - **overlays.nix**: Package overlays and customizations

### Darwin Configurations
Multiple machine-specific configurations in `darwinConfigurations`:
- `yqrashawn` - Main aarch64-darwin setup
- `mini`, `studio`, `mbp` - Specialized aarch64-darwin machines
- `yqrashawn-intel` - x86_64-darwin setup
- `holybasil` - Work machine with separate profile
- `work` - Minimal work configuration

### Home Manager Integration
- Embedded within darwin configurations via `home-manager.darwinModules.home-manager`
- Manages user dotfiles, CLI tools, and applications
- Includes extensive Doom Emacs configuration in `.doom.d/`

### Module Organization
- **CLI tools**: Individual modules for bat, fzf, starship, tmux, zoxide, etc.
- **Dotfiles**: Comprehensive configuration for alacritty, karabiner, rime input method
- **Development**: Languages support via asdf plugins (node, python, clojure, etc.)

### Special Features
- **Homebrew integration**: Manages GUI apps via homebrew module
- **gptel-tools**: Custom Emacs tools for AI integration (buffer.el, elisp.el)
- **Babashka libraries**: Clojure utilities for system automation
- **Extensive snippets**: YASnippet templates for multiple languages

## File Locations

### Configuration Entry Points
- System config: `profiles/yqrashawn.nix` → `modules/yqrashawn/darwin/`
- User config: `profiles/home-manager/yqrashawn.nix` → `modules/yqrashawn/home-manager/`

### Key Dotfiles
- Doom Emacs: `.doom.d/` (symlinked via install.sh)
- CLI configs: `modules/yqrashawn/home-manager/dotfiles/`
- Alacritty: `modules/yqrashawn/home-manager/dotfiles/alacritty/`
- Karabiner: `modules/yqrashawn/home-manager/dotfiles/karabiner.edn`

### Development Tools
- Formatters: `treefmt.toml` (nixpkgs-fmt, black, stylua)
- Python scripts: `bin/do.py` (main automation)
- Shell utilities: `bin/bootstrap.sh`, `install.sh`, `update-nix.sh`

## Platform Support

The repository automatically detects platform:
- **Darwin**: Uses `darwinConfigurations` with nix-darwin
- **NixOS**: Would use `nixosConfigurations` (currently unused)
- **Other Linux**: Would use `homeConfigurations` (currently commented out)

All configurations use nixpkgs-unstable by default with additional inputs for specific tools and overlays.

## Check <project-root>/CLAUDE.md for Doom Emacs Configuration (.doom.d/)
