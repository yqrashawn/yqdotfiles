alias d='cd ~/Downloads/'
alias wo='cd ~/workspace/OFFICE/'
alias wt='cd ~/workspace/THIRD/'
alias wh='cd ~/workspace/HOME/'
alias rg="rg --smart-case"
alias rgu="rg -uu"
alias rgfu="rg -F -u"
alias rgf="rg --files --no-ignore --hidden --follow"
alias rgfh="rg --files --no-ignore --hidden --follow ./"
alias zc="emacsclient -n  ~/.zshrc"
alias ec="cd ~/.emacs.d/ && gws && gwd"
alias cl='clear'
alias o='open'
alias p2='percol'
alias upgradenix="sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'"

alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"


# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"


alias iconfig='ifconfig | awk '\''{if ( $1 >= "en" && $2 >= "flags" && $3 == "mtu") {print $1}; if ( $1 == "inet" || $1 == "status:"){print $0};}'\''|egrep "en|lo|inet"'
# check network state
alias ifactive="ifconfig | pcregrep -M -o '^[^\t:]+:([^\n]|\n\t)*status: active'"
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

alias l='exa -al'
alias ls="exa -a"
alias lsa="exa -abghl --git --color=automatic"
alias lsd="exa -l --color=automatic | grep --color=never '^d'"
alias lst='exa --sort=created --time=created --long --all -r | sed 15q'
alias lstt='exa --sort=modified --time=created --long --all -r | sed 15q'
# alias ls="ls -AlF ${colorflag}"
# alias l="ls -lF ${colorflag}"
# alias lsd="ls -lF ${colorflag} | grep --color=never '^d'"
alias reload="exec $SHELL -l"


# servers
alias rbserv='ruby -run -e httpd . -p 8000'
alias rbservp='ruby -run -e httpd . -p '
alias ttmux='tmuxinator start projects'
# alias server='http-server'
alias serverp='http-server -p'
alias serv="live-server --port=8081"
# alias serv="devd -ol ."
alias httpg="http-server -g"

# editors
# alias e="emacsclient -n"
# alias ee='emacsclient -nw'
# alias eee='emacsclient -nw'
alias demacs='emacs -Q -l ~/.emacs.d/init.debug.el'
alias v='nvim'
alias pyou='you-get -p mpv'
alias mmpv='mpv -vo=opengl '
alias vv='fasd -f -e nvim '
alias t='emacsclient -t'
alias vvv='vimr'
alias m='mvim'
alias wdired='roamer'
alias emacsd='vmtouch -ef ~/.emacs.d/ && vmtouch -tldf ~/.emacs.d/ && vmtouch -ef /Applications/Emacs.app/ && vmtouch -tldf /Applications/Emacs.app/'

function mlock {
  vmtouch -ef $@ && vmtouch -tldf $@
}

alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'
alias chromec='/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary'
alias chromed='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-debugging-port=9222'
alias mongodd='mongod -dbpath ~/workspace/projects/mongodb/data'

# alias ss='export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152 & '
# alias ss='export https_proxy=http://127.0.0.1:6152;export all_proxy=http://127.0.0.1:6152 & '
alias s='proxychains4 -f ~/Dropbox/sync/proxychains.conf'
alias ss='export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152;export HTTP_PROXY=http://127.0.0.1:6152:export HTTPs_PROXY=http://127.0.0.1:6152 &'
alias sf='proxychains4 -f /etc/proxychains.conf'

alias aria2c='aria2c --enable-rpc --rpc-listen-all'

# macOS has no `md5sum`, so use `md5` as a fallback
command -v md5sum > /dev/null || alias md5sum="md5"

# macOS has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum > /dev/null || alias sha1sum="shasum"

# misc
alias rm='trash'
alias fixbrew='chown -R `whoami`:admin $(brew --prefix)'

# env
alias eenv='printenv | fzf'

# fd
alias fdf='fd -t f'
alias fdd='fd -t d'
alias fdfe='fd -t f -e'

# tmp
# alias yarn='cross-env NPM_CONFIG_PYTHON="/usr/bin/python" yarn'
alias wifi-device-name='networksetup -listallhardwareports'

# reflex
alias ref='reflex -c reflex.conf'

# dired
alias dired="emacsclient -a '' -t -e '(my-dired-frame default-directory)'"

# png
function pngopti {
  optipng -o7 $@
}
function pngq {
  pngquant --force --speed 1 --output $@ $@
}
function opng {
  pngq $@
  pngopti $@
}
function pnghalf {
  convert -resize 50% $@ $@
}
function opnghalf {
  convert -resize 50% $@ $@
  pngq $@
  pngopti $@
}

# update email
alias umail="proxychains4 -f /etc/proxychains.conf mbsync -a"

# updo git push
alias undopush="git push -f origin HEAD^:master"
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias cleanup_dsstore="find . -name '*.DS_Store' -type f -ls -delete"
alias ungz='gunzip -k'

#diff
alias diff='diff -Naurbw'
alias ssh='TERM=xterm-256color ssh'

# tmux
alias tma="tmux a -t $1"
alias tml='tmux list-sessions'
alias tmn='tmux new -s $1'
alias tmC='tmux kill-session -a'

# yarn
# alias y="yarn"
# https://nobitagit.github.io/blog/Yarn-Npm-Faster/
function y {
  mkdir node_modules 2>/dev/null
  touch ./node_modules/.metadata_never_index
  yarn $@
}
alias ya="yarn add"
alias yad="yarn add --dev"
alias yap="yarn add --peer"
alias yb="yarn build"
alias ycc="yarn cache clean"
alias yga="yarn global add"
alias ygx="yarn global remove"
alias ygls="yarn global list"
alias ygrm="yarn global remove"
alias ygu="yarn global upgrade"
alias yh="yarn help"
alias yI="yarn init"
alias yls="yarn list"
alias yout="yarn outdated"
alias yP="yarn pack"
alias yx="yarn remove"
alias yr="yarn run"
alias yd="yarn dev"
alias ys="yarn serve"
alias yst="yarn start"
alias yt="yarn test"
alias yuc="yarn global upgrade && yarn cache clean"
alias yui="yarn upgrade-interactive"
alias yup="yarn upgrade"

# geth local dev
alias gethld="geth --rpc --dev --miner.threads 1 console"