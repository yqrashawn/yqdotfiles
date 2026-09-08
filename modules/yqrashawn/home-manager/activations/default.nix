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
    # pr-review-loop: `rtk init --global --auto-patch` above re-registers rtk's
    # own Bash PreToolUse hook. The push-directory recorder needs the wrapper in
    # that slot instead: it delegates to rtk-rewrite.sh unchanged, then appends a
    # $PWD recorder to `git push` / `gh pr create` commands, because the hook
    # payload never carries the directory the command actually ran in. See
    # .superpowers/sdd/2026-09-08-pr-review-loop/rtk-wrapper-report.md.
    # Re-apply that swap idempotently. Deliberately not nested in the
    # `command -v rtk` guard above: ~/.claude/settings.json is Dropbox-synced, so
    # the entry is worth checking even on a machine that has no rtk.
    prl_settings=~/.claude/settings.json
    prl_wrapper=~/.claude/hooks/rtk-rewrite-wrapper.sh
    prl_jq="$(command -v jq || true)"
    [ -x "$prl_jq" ] || prl_jq=/etc/profiles/per-user/"$cuser"/bin/jq
    prl_count='[.hooks.PreToolUse[]?.hooks[]?|select((.command // "")|endswith($s))]|length'
    if [ ! -f "$prl_settings" ]; then
        echo "WARNING: pr-review-loop hook swap skipped: $prl_settings not found" >&2
    elif [ ! -x "$prl_wrapper" ]; then
        echo "WARNING: pr-review-loop hook swap skipped: expected an executable hook at $prl_wrapper. rtk's own hook stays registered and push directories will NOT be recorded." >&2
    elif [ ! -x "$prl_jq" ]; then
        echo "WARNING: pr-review-loop hook swap skipped: no jq on PATH nor at $prl_jq" >&2
    else
        prl_n_rtk="$("$prl_jq" --arg s rtk-rewrite.sh "$prl_count" "$prl_settings" || echo -1)"
        prl_n_wrap="$("$prl_jq" --arg s rtk-rewrite-wrapper.sh "$prl_count" "$prl_settings" || echo -1)"
        if [ "$prl_n_rtk" = -1 ] || [ "$prl_n_wrap" = -1 ]; then
            echo "WARNING: pr-review-loop hook swap skipped: could not read hooks.PreToolUse out of $prl_settings" >&2
        elif [ "$prl_n_wrap" -gt 1 ]; then
            echo "WARNING: pr-review-loop hook swap skipped: $prl_n_wrap rtk-rewrite-wrapper.sh entries are registered in $prl_settings, expected exactly 1. Remove the duplicates by hand; the wrapper is running more than once per Bash call." >&2
        elif [ "$prl_n_rtk" = 0 ] && [ "$prl_n_wrap" = 0 ]; then
            echo "WARNING: pr-review-loop hook swap found nothing to swap: expected a hooks.PreToolUse entry in $prl_settings whose command ends in rtk-rewrite.sh (rtk's, to be replaced) or rtk-rewrite-wrapper.sh (already swapped), and found neither. The hook layout changed; push directories will NOT be recorded." >&2
        elif [ "$prl_n_rtk" = 0 ]; then
            : # already swapped, nothing to do
        else
            if [ "$prl_n_wrap" = 1 ]; then
                # wrapper still registered, so rtk spliced a duplicate entry: drop rtk's
                prl_new="$("$prl_jq" '.hooks.PreToolUse |= ( [ .[] | .hooks |= map(select((.command // "") | endswith("rtk-rewrite.sh") | not)) ] | map(select((.hooks | length) > 0)) )' "$prl_settings" || true)"
            else
                # rtk took the slot back: point it at the wrapper again, in place
                prl_new="$("$prl_jq" --arg w "$prl_wrapper" '.hooks.PreToolUse |= [ .[] | .hooks |= ( map(if (.command // "") | endswith("rtk-rewrite.sh") then .command = $w else . end) | reduce .[] as $h ([]; if ($h.command == $w and ((map(.command) | index($w)) != null)) then . else . + [$h] end) ) ]' "$prl_settings" || true)"
            fi
            # only write if the result really does register the wrapper
            if [ -n "$prl_new" ] && printf '%s' "$prl_new" | "$prl_jq" -e --arg w "$prl_wrapper" 'any(.hooks.PreToolUse[]?.hooks[]?; .command == $w)' > /dev/null; then
                printf '%s\n' "$prl_new" > "$prl_settings"
                echo 'pr-review-loop: re-pointed the Bash PreToolUse hook at rtk-rewrite-wrapper.sh'
            else
                echo "WARNING: pr-review-loop hook swap FAILED: jq could not rewrite $prl_settings. rtk's own hook stays registered and push directories will NOT be recorded." >&2
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
