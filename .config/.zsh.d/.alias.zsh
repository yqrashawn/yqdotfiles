alias d='cd ~/Downloads/'
alias w='cd ~/workspace/'
alias wo='cd ~/workspace/OFFICE/'
alias wt='cd ~/workspace/THIRD/'
alias wh='cd ~/workspace/HOME/'
alias rg="rg --smart-case"
alias rgu="rg -uu"
alias rgfu="rg -F -u"
alias rgf="rg --files --no-ignore --hidden --follow"
alias rgfh="rg --files --no-ignore --hidden --follow ./"
alias zc="emacsclient ~/.zshrc"
alias zcc="emacsclient ~/.zprezto/runcoms/zpreztorc"
alias cl='clear'
alias o='open'
alias p2='percol'

alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"

alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'

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
alias httpg="http-server -g"

# editors
alias e='emacsclient -n'
alias ee='emacsclient -nw'
alias eee='fasd -f -e emacsclient -nw'
alias demacs='emacs -Q -l ~/.emacs.d/init.debug.el'
alias v='nvim'
alias pyou='you-get -p mpv'
alias mmpv='mpv -vo=opengl '
alias vv='fasd -f -e nvim'
alias t='emacsclient -t'
alias vvv='vimr'
alias m='mvim'
alias wdired='roamer'
alias emacsd='vmtouch -ef ~/.emacs.d/ && vmtouch -tldf ~/.emacs.d/'

alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'
alias chromed='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-debugging-port=9222'
alias mongodd='mongod -dbpath ~/workspace/projects/mongodb/data'

# alias ss='export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152 & '
# alias ss='export https_proxy=http://127.0.0.1:6152;export all_proxy=http://127.0.0.1:6152 & '
alias ss='export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152;export HTTP_PROXY=http://127.0.0.1:6152:export HTTPs_PROXY=http://127.0.0.1:6152 &'

# douyu
alias d820='mpv https://www.douyu.com/zyt820'
alias dyyf='mpv https://www.douyu.com/58428'
alias dchuan='mpv https://www.douyu.com/chuan967'
alias dxiao8='mpv https://www.douyu.com/xiao8'

alias aria2c='aria2c --enable-rpc --rpc-listen-all'

# macOS has no `md5sum`, so use `md5` as a fallback
command -v md5sum > /dev/null || alias md5sum="md5"

# macOS has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum > /dev/null || alias sha1sum="shasum"

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
alias pngq="pngquant -f --speed 1"
alias optipng="optipng -o7"

# update email
alias umail="proxychains4 -f /etc/proxychains.conf mbsync gmail"

# updo git push
alias undopush="git push -f origin HEAD^:master"
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias cleanup_dsstore="find . -name '*.DS_Store' -type f -ls -delete"
alias ungz='gunzip -k'

alias ssh='TERM=xterm-256color ssh'