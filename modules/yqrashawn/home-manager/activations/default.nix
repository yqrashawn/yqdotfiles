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
    # pr-review-loop: `rtk init --global --auto-patch` above regenerates rtk's own
    # Bash PreToolUse hook and re-registers it, which silently undoes this swap on
    # every rebuild. The push-directory recorder needs the wrapper in that slot
    # instead: it delegates to rtk-rewrite.sh unchanged, then wraps a `git push` /
    # `gh pr create` subcommand in `{ recorder; push; }` so the shell itself
    # reports the directory the push really ran in -- the hook payload never
    # carries it. See .superpowers/sdd/2026-09-08-pr-review-loop/wrapper-v2-report.md.
    # Deliberately not nested in the `command -v rtk` guard above:
    # ~/.claude/settings.json is Dropbox-synced, so the entry is worth keeping
    # correct even on a machine that has no rtk.
    prl_src=~/.nixpkgs/claude-code-plugins/hooks/rtk-rewrite-wrapper.sh
    prl_dst=~/.claude/hooks/rtk-rewrite-wrapper.sh
    prl_settings=~/.claude/settings.json
    prl_jq="$(command -v jq || true)"
    [ -x "$prl_jq" ] || prl_jq=/etc/profiles/per-user/"$cuser"/bin/jq
    prl_count='[.hooks.PreToolUse[]?.hooks[]?|select((.command // "")|endswith($s))]|length'
    if [ ! -f "$prl_src" ]; then
        echo "WARNING: pr-review-loop: tracked wrapper $prl_src is missing. rtk's own hook stays registered and push directories will NOT be recorded." >&2
    elif [ ! -d ~/.claude/hooks ]; then
        echo "WARNING: pr-review-loop: ~/.claude/hooks does not exist yet, so the wrapper cannot be installed. Expected only before the ~/.claude symlinks below have ever been created -- re-run the activation." >&2
    elif [ ! -f "$prl_settings" ]; then
        echo "WARNING: pr-review-loop: $prl_settings not found, so the PreToolUse hook cannot be re-pointed. Push directories will NOT be recorded." >&2
    elif [ ! -x "$prl_jq" ]; then
        echo "WARNING: pr-review-loop: no jq on PATH nor at $prl_jq, so the PreToolUse hook cannot be re-pointed. Push directories will NOT be recorded." >&2
    else
        # cp through the symlink, never ln -s: ~/.claude/hooks points into Dropbox
        # and other machines read this file. cmp first so an unchanged wrapper
        # does not churn Dropbox on every rebuild.
        if ! cmp -s "$prl_src" "$prl_dst"; then
            echo 'install ~/.claude/hooks/rtk-rewrite-wrapper.sh'
            cp -f "$prl_src" "$prl_dst"
            chmod +x "$prl_dst"
        fi
        prl_w="$("$prl_jq" --arg s rtk-rewrite-wrapper.sh "$prl_count" "$prl_settings" 2>/dev/null || echo -1)"
        prl_r="$("$prl_jq" --arg s rtk-rewrite.sh "$prl_count" "$prl_settings" 2>/dev/null || echo -1)"
        if [ "$prl_w" = -1 ] || [ "$prl_r" = -1 ]; then
            echo "WARNING: pr-review-loop: could not read hooks.PreToolUse out of $prl_settings. Push directories will NOT be recorded." >&2
        elif [ "$prl_w" -gt 1 ] || [ "$prl_r" -gt 1 ]; then
            echo "WARNING: pr-review-loop: $prl_settings registers $prl_w rtk-rewrite-wrapper.sh and $prl_r rtk-rewrite.sh PreToolUse entries; at most one of each is expected. Fix it by hand -- the hook is running more than once per Bash call." >&2
        elif [ "$prl_w" = 1 ] && [ "$prl_r" = 0 ]; then
            : # already swapped
        elif [ "$prl_w" = 0 ] && [ "$prl_r" = 0 ]; then
            echo "WARNING: pr-review-loop: no hooks.PreToolUse entry in $prl_settings ends in rtk-rewrite.sh (rtk's, to be replaced) or rtk-rewrite-wrapper.sh (already swapped). The hook layout changed; push directories will NOT be recorded." >&2
        else
            if [ "$prl_w" = 1 ]; then
                # wrapper still registered, so rtk spliced a duplicate entry: drop rtk's
                prl_new="$("$prl_jq" '.hooks.PreToolUse |= (map(.hooks |= map(select((.command // "")|endswith("rtk-rewrite.sh")|not))) | map(select((.hooks|length) > 0)))' "$prl_settings" || true)"
            else
                # rtk took the slot back: re-point that entry at the wrapper, in place
                prl_new="$("$prl_jq" --arg w "$prl_dst" '.hooks.PreToolUse |= map(.hooks |= map(if (.command // "")|endswith("rtk-rewrite.sh") then .command = $w else . end))' "$prl_settings" || true)"
            fi
            # write only when the result really does register the wrapper exactly once and rtk's hook not at all
            if [ -n "$prl_new" ] && printf '%s' "$prl_new" | "$prl_jq" -e --arg w "$prl_dst" '([.hooks.PreToolUse[]?.hooks[]?|select(.command == $w)]|length) == 1 and ([.hooks.PreToolUse[]?.hooks[]?|select((.command // "")|endswith("rtk-rewrite.sh"))]|length) == 0' > /dev/null; then
                printf '%s\n' "$prl_new" > "$prl_settings"
                echo 'pr-review-loop: re-pointed the Bash PreToolUse hook at rtk-rewrite-wrapper.sh'
            else
                echo "WARNING: pr-review-loop: jq could not rewrite $prl_settings. rtk's own hook stays registered and push directories will NOT be recorded." >&2
            fi
        fi
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
  '';
}
