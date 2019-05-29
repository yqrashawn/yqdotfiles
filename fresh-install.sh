#!/usr/bin/env bash

cd
xcode-select --install
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
git clone https://github.com/yqrashawn/.emacs.d.git
brew cask install karabiner-elements emacs-nightly
git clone https://github.com/yqrashawn/yqdotfiles.git
brew cask install google-chrome
brew install yqrashawn/goku/goku
brew services start goku
brew cask install alacritty iina notion podcastmenu bitbar key-codes qlmarkdown visual-studio-code quitter kitematic spectacle fiscript squirrel flux mpv telegram the-unarchiver setapp kawa anybar
brew install ack ffmpeg openssl sqlite html2text optipng sshrc findutils htop flac httpie hub p7zip fontconfig pandoc aria2 imagemagick pcre autoconf libyaml pcre2 terminal-notifier autoconf-archive fx imapfilter libzip perl autojump fzf pinentry texi2html automake lua pinentry-mac texinfo bash the_platinum_searcher bash-completion gcc luajit pkg-config the_silver_searcher ispell lumo planck thefuck bfg isync bfs m-cli pngquant tmate macvim poppler tmux getwindowid jid mailcheck topgrade mailutils jpeg proxychains-ng trash brew-cask-completion maven proxytunnel chicken git languagetool mongodb chrome-cli git-extras leiningen pyenv chunkwm git-flow mpfr unixodbc clang-format git-lfs less mpv cliclick git-standup python clipper python3 clojure multimarkdown clojurescript mycli wabt cmake n coreutils qt cscope ctags neovim ranger wget curl readline reattach-to-user-namespace redis diff-so-fancy gnu-sed nginx ripgrep gnupg dive gnupg2 nnn gnutls libogg node yarn doxygen go ruby editorconfig rust you-get goku youtube-dl gpg-agent exa zsh fasd grep fcitx-remote-for-osx fd

git clone --recursive https://github.com/yqrashawn/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done

# https://stackoverflow.com/questions/31034870/making-zsh-default-shell-in-macosx
# or prezto won't load
sudo echo "$(which zsh)" >> /etc/shells
chsh -s /usr/local/bin/zsh
