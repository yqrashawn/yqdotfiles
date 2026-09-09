{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.activation.defaultSetup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    export PATH=/run/current-system/sw/bin:$PATH
    cuser="$(id -un)"

    if [ ! -e ~/.doom.d ]; then
        ln -s ~/.nixpkgs/.doom.d ~/.doom.d
    fi

    if [ ! -e ~/.local/share/yarn/global/package.json ]; then
        mkdir -p ~/.local/share/yarn/global
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yarn-package.json ~/.local/share/yarn/global/package.json
    fi

    if [ ! -e ~/.local/share/pnpm/global/5/package.json ]; then
        mkdir -p ~/.local/share/pnpm/global/5
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/pnpm-package.json ~/.local/share/pnpm/global/5/package.json
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/pnpm-lock.yaml ~/.local/share/pnpm/global/5/pnpm-lock.yaml
    fi

    if [ ! -e ~/.cache/.bun/install/global/package.json ]; then
        mkdir -p ~/.cache/.bun/install/global
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/bun-package.json ~/.cache/.bun/install/global/package.json
    fi

    if [ ! -e ~/.config/karabiner.edn ]; then
      if [ "$cuser" = "holybasil" ]; then
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/hkarabiner.edn ~/.config/karabiner.edn
      else
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/karabiner.edn ~/.config/karabiner.edn
      fi
    fi

    if [ ! -e ~/.config/yabai ]; then
      rm -rf ~/.config/yabai || true
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai ~/.config/yabai
    fi

    if [ ! -e ~/Dropbox ]; then
      rm -rf ~/Dropbox || true
      ln -s ~/Library/CloudStorage/Dropbox ~/Dropbox
    fi

    if [ ! -e ~/.tridactylrc ]; then
      rm -rf ~/.tridactylrc || true
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.tridactylrc ~/.tridactylrc
    fi

    if [ ! -e ~/.authinfo.gpg ] && [ "$cuser" = "yqrashawn" ]; then
      rm -rf ~/.authinfo.gpg || true
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.authinfo.gpg ~/.authinfo.gpg
    fi

    if [ ! -e ~/.spacehammer ]; then
      rm -rf ~/.spacehammer || true
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.spacehammer ~/.spacehammer
    fi

    if [ ! -e ~/.gitignore_global ]; then
      rm -rf ~/.gitignore_global || true
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/gitignore_global ~/.gitignore_global
    fi

    ln -fs /Applications/Nix\ Apps/* /Applications/

    if [ -e ~/Dropbox/sync/oauth2token ] && [ ! -e ~/.local/share/oauth2token ]; then
        echo 'link oauth2token, pip install oauth2token'
        ln -s ~/Dropbox/sync/oauth2token ~/.local/share/oauth2token
    fi
    if [ -e ~/Dropbox/sync/ntf ] && [ ! -e ~/local/bin/ntf ]; then
        echo 'link ~/local/bin/ntf'
        ln -s ~/Dropbox/sync/ntf ~/local/bin/ntf
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
    if [ ! -e ~/.cargo/config.toml ]; then
        echo 'link cargo conf'
        mkdir -p ~/.cargo/
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/cargo-config.toml ~/.cargo/config.toml
    fi
    if [ ! -e ~/.config/nyxt/config.lisp ]; then
        echo 'link ~/.config/nyxt/config.lisp'
        mkdir -p ~/.config/nyxt
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/nyxt.lisp ~/.config/nyxt/config.lisp
    fi
    if [ ! -e ~/.config/ai-behaviors ]; then
        ln -s ~/Dropbox/sync/ai-behaviors/ ~/.config/
    fi
    if command -v rtk &> /dev/null; then
        echo 'rtk init --global --auto-patch'
        rtk init --global --auto-patch
    fi
    if command -v ~/.asdf/shims/clojure &> /dev/null; then
        ~/.asdf/shims/clojure -Ttools install-latest :lib io.github.bhauman/clojure-mcp :as mcp                        
    fi
    if [ ! -e ~/.claude/hooks ] &> /dev/null; then
        mkdir -p ~/.claude
        ln -s ~/Dropbox/sync/claude-settings.json ~/.claude/settings.json
        ln -s ~/Dropbox/sync/claude-memory.json ~/.claude/CLAUDE.md
        ln -s ~/Dropbox/sync/claude-commands ~/.claude/commands
        ln -s ~/Dropbox/sync/claude-hooks ~/.claude/hooks
        ln -s ~/Dropbox/sync/claude-skills ~/.claude/skills
        ln -s ~/Dropbox/sync/claude-agents ~/.claude/agents
        ln -s ~/Dropbox/sync/claude-plugins/installed_plugins.json ~/.claude/plugins/installed_plugins.json
        ln -s ~/Dropbox/sync/claude-plugins/known_marketplaces.json ~/.claude/plugins/known_marketplaces.json
    fi

    # pr-review-loop: the plugin's SOURCE travels with this repo, and
    # settings.json enables it, but the plugin CACHE that Claude Code actually
    # loads is per-machine and nothing else populates it. Without this a fresh
    # machine has the loop enabled and silently reviews nothing -- which is
    # the failure mode this whole plugin exists to stop having.
    #
    # Deliberately AFTER the ~/.claude symlink block above: `claude plugin
    # marketplace add` writes ~/.claude/plugins/known_marketplaces.json, and a
    # real file there would make that block's `ln -s` fail.
    if ! command -v bb &> /dev/null; then
        echo "WARNING: pr-review-loop: no bb (babashka) on PATH. hooks.json runs a bare \`bb\`, so every review would fail silently. Install it with asdf." >&2
    fi
    prl_dir=~/.nixpkgs/claude-code-plugins
    prl_manifest="$prl_dir/.claude-plugin/plugin.json"
    if [ ! -f "$prl_manifest" ]; then
        echo "WARNING: pr-review-loop: $prl_manifest is missing, so the plugin cannot be installed." >&2
    elif ! command -v claude &> /dev/null; then
        echo "WARNING: pr-review-loop: no claude on PATH, so the plugin cache cannot be populated. The loop will not run." >&2
    else
        prl_ver=$(sed -n 's/.*"version"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$prl_manifest" | head -1)
        prl_cached=~/.claude/plugins/cache/nixpkgs-plugins/pr-review-loop/"$prl_ver"
        if [ -z "$prl_ver" ]; then
            echo "WARNING: pr-review-loop: no version in $prl_manifest, so the cache cannot be checked. Skipping." >&2
        elif [ -d "$prl_cached" ]; then
            : # the cache already holds this exact version
        else
            echo "pr-review-loop: installing $prl_ver"
            # add is a no-op when the marketplace is known; install is a no-op
            # when already installed but does NOT upgrade, so update follows it.
            claude plugin marketplace add "$prl_dir" > /dev/null 2>&1 || true
            claude plugin install pr-review-loop@nixpkgs-plugins > /dev/null 2>&1 || true
            claude plugin update pr-review-loop > /dev/null 2>&1 || true
            if [ ! -d "$prl_cached" ]; then
                echo "WARNING: pr-review-loop: $prl_ver is still not in the plugin cache after install and update. The review loop will not run on this machine." >&2
            fi
        fi
    fi
  '';
}
