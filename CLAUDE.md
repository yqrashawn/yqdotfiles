# CLAUDE.md

This file provides guidance to LLM when working with code in `<project-root>/.doom.d` in this repository.

## Doom Emacs Configuration (.doom.d/)

The `.doom.d/` directory contains a comprehensive Doom Emacs configuration with extensive customizations:

### Instructions
- use `get_buffer_file_path` tool to get absolute path of a buffer
- whenever working on emacs lisp buffer `foo.el`, evaluate the buffer after changes made, check `foo-test.el` at the same directory see if corresponding test is missing or outdated

### Run Tests
- use run ert test tool to run elisp ert tests
- evaluate test file before run the test, test files are not loaded by default

### Core Files
- **init.el**: Doom module configuration and feature flags
- **config.el**: Main configuration with fonts, themes, and personal settings
- **packages.el**: Additional package declarations and overrides

### Language-Specific Configurations
- **clojure.el**: Clojure/ClojureScript development setup
- **js.el**: JavaScript/TypeScript configuration
- **golang.el**: Go language support
- **lisp.el** / **clisp.el**: Common Lisp development
- **lang.el**: General language configurations

### Feature Modules
- **gpt.el** / **llm.el**: AI integration and language model tools
- **gptel-tools.el**: Custom tools for AI-powered development
- **org.el**: Org-mode configuration for note-taking and productivity
- **mail.el**: Email configuration
- **completion.el**: Code completion and snippets
- **visual.el**: UI and appearance customizations

### Development Tools
- **prog.el**: General programming utilities
- **version-control.el**: Git and version control integration
- **navigation.el**: File and buffer navigation enhancements
- **better-default.el**: Improved default behaviors

### Custom Tools
- **gptel-tools/**: Directory containing custom AI integration tools definition:
  - **utils.el**: Utilities for other tools
  - **todo.el**: Read/Write todo tools
  - **buffer.el**: Buffer manipulation tools
  - **elisp.el**: Emacs Lisp development utilities
  - **read.el**: File reading tools
  - **edit-file.el**: File editing utilities
  - **create-file.el**: File creation tools
  - **ripgrep.el**: Search integration
  - **clj.el**: Clojure development
  - **cljs.el**: Clojurescript development
  - Corresponding test files for each tool

### Snippets
Extensive YASnippet template collection in `snippets/` for:
- Clojure/ClojureScript development
- JavaScript/TypeScript/React
- Git commit messages (conventional commits)
- Emacs Lisp
- General programming constructs

### Configuration Management
- **autoload.el**: Autoloaded functions
- **helper.el**: Utility functions
- **map.el**: Keybinding definitions
- **custom.el**: Customize interface settings
- **pragmatapro-prettify-symbols.el**: Font ligature configuration

The Emacs configuration is symlinked from the repository root via `install.sh` and integrates with the broader Nix-managed development environment.
