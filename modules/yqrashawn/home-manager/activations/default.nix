{ config, lib, pkgs, ... }:

{
  home.activation.defaultSetup = lib.hm.dag.entryAfter ["writeBoundary"] ''
    cuser="$(id -un)"

    rm -rf ~/.doom.d || true
    ln -s ~/.nixpkgs/.doom.d ~/.doom.d

    if [ ! -e ~/.local/share/yarn/global/package.json ]; then
        mkdir -p ~/.local/share/yarn/global
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yarn-package.json ~/.local/share/yarn/global/package.json
    fi

    if [ ! -e ~/.local/share/pnpm/global/5/package.json ]; then
        mkdir -p ~/.local/share/pnpm/global/5
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/pnpm-package.json ~/.local/share/pnpm/global/5/package.json
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/pnpm-lock.yaml ~/.local/share/pnpm/global/5/pnpm-lock.yaml
    fi

    rm -rf ~/.config/karabiner.edn || true

    if [ "$cuser" = "holybasil" ]; then
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/hkarabiner.edn ~/.config/karabiner.edn
    else
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/karabiner.edn ~/.config/karabiner.edn
    fi

    rm -rf ~/.config/yabai || true
    ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/yabai ~/.config/yabai

    rm -rf ~/Dropbox || true
    ln -s ~/Library/CloudStorage/Dropbox ~/Dropbox

    rm -rf ~/.tridactylrc || true
    ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.tridactylrc ~/.tridactylrc

    # rm -rf ~/.tridactyl || true
    # ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/tridactyl ~/.tridactyl

    if [ "$cuser" = "yqrashawn" ]; then
      rm -rf ~/.authinfo.gpg || true
      ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.authinfo.gpg ~/.authinfo.gpg
    fi

    rm -rf ~/.spacehammer || true
    ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.spacehammer ~/.spacehammer

    rm -rf ~/.gitignore_global || true
    ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/gitignore_global ~/.gitignore_global

    if [ "$cuser" = "yqrashawn" ]; then
      rm ~/.ssh/config || true
      ${pkgs.gnupg}/bin/gpg --decrypt --output  ~/.ssh/config ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/ssh.gpg || true

      rm ~/.mbsyncrc || true
      ${pkgs.gnupg}/bin/gpg --decrypt --output  ~/.mbsyncrc ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/.mbsyncrc.gpg || true
    fi

    ln -fs /Applications/Nix\ Apps/* /Applications/

    if [ -e ~/Dropbox/sync/oauth2token ] && [ ! -e ~/.local/share/oauth2token ]; then
        echo 'link oauth2token, pip install oauth2token'
        ln -s ~/Dropbox/sync/oauth2token ~/.local/share/oauth2token
    fi
    if [ -e ~/Dropbox/sync/ntf ] && [ ! -e /opt/homebrew/bin/ntf ]; then
        echo 'link /opt/homebrew/bin/ntf'
        ln -s ~/Dropbox/sync/ntf /opt/homebrew/bin/ntf
    fi
    if [ -e ~/Dropbox/sync/.ntf.yml ] && [ ! -e ~/.ntf.yml ]; then
        echo 'link ~/.ntf.yml'
        ln -s ~/Dropbox/sync/.ntf.yml ~/.ntf.yml
    fi
    if [ -e ~/Dropbox/sync/.notmuch-config ] && [ ! -e ~/.notmuch-config ]; then
        echo 'link .notmuch-config'
        ln -s ~/Dropbox/sync/.notmuch-config ~/.notmuch-config
    fi
    if [ -e ~/Dropbox/sync/.msmtprc ] && [ ! -e ~/.msmtprc ]; then
        echo 'link .msmtprc'
        ln -s ~/Dropbox/sync/.msmtprc ~/.msmtprc
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
    if [ ! -e ~/.config/nyxt/config.lisp ]; then
        echo 'link ~/.config/nyxt/config.lisp'
        mkdir -p ~/.config/nyxt
        ln -s ~/.nixpkgs/modules/yqrashawn/home-manager/dotfiles/nyxt.lisp ~/.config/nyxt/config.lisp
    fi
  '';
}
