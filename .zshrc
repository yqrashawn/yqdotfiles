#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi
# Customize to your needs...
#

# 10ms for key sequences
KEYTIMEOUT=1

#for pj
PROJECT_PATHS=(~/workspace ~/workspace/projects ~/workspace/XiheMap ~/workspace/gogs-webhook ~/workspace/projecs/cesium)

autoload -U compinit && compinit


# export PATH=$HOME/bin:/usr/local/bin:$PATH


# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='nvim'
else
    export EDITOR='nvim'
fi

# make sure that if a program wants you to edit
# text, that Vim is going to be there for you
# export EDITOR="vim"
export USE_EDITOR=$EDITOR
export VISUAL=$EDITOR
# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#

alias vimdiff='nvim -d'
alias v2ex='v2ex-go'

alias zc="nvim ~/.zshrc"
alias zcc="nvim ~/.zprezto/runcoms/zpreztorc"
alias ggit='o ~/.zprezto/modules/git/README.md'
alias cl='clear'
alias o='open'
alias lst='ls -t'

alias karaconfig='nvim /home/yqrashawn/Library/Application\ Support/Karabiner/private.xml'
alias proxyconfig='nvim /usr/local/etc/privoxy/config'
alias grep='grep -r'
alias ackl='ack -l'
alias sss='proxychains4'
alias rrm='rm *.*~ && rm .*~'
alias l='ls -l -A'
alias v.='v .'
alias v='nvim'
alias p='you-get -p mpv '
alias vv='fasd -f -e nvim'
alias c='fasd_cd -i'
alias finalz='p http://www.douyutv.com/D3D3D3'
alias vimconfig='nvim /home/yqrashawn/.config/nvim/init.vim'
alias ariaconfig='nvim /home/yqrashawn/.aria2/aria2.conf'
alias ttmux='tmuxinator start projects'
alias tmuxconfig='nvim ~/.tmux.conf'
alias tls='tmux ls'
alias surconfig='nvim ~/.surge.conf'
alias cyper='p http://www.douyutv.com/576190'
alias server='http-server'
alias serverp='http-server -p'
alias iss='instantmusic -s '
alias findh='mdfind -onlyin . '
alias gitshadow="git config --global http.proxy 'socks5://127.0.0.1:1080' \
    git config --global https.proxy 'socks5://127.0.0.1:1080'"
alias gitunshadow="git config --global --unset http.proxy \
    git config -'global --unset https.proxy"
alias banwa='ssh root@45.78.62.230 -p 26812'
alias mongodd='mongod -dbpath ~/workspace/projects/mongodb/data'
alias ss='export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152 & '

alias gff='git flow feature'
alias gffs='git flow feature start'
alias gfff='git flow feature finish'
alias gffp='git flow feature publish'
alias gfft='git flow feature track'
alias gffd='git flow feature diff'
alias gffr='git flow feature rebase'
alias gffc='git flow feature checkout'
alias gffd='git flow feature delete'
alias gfb='git flow  bugfix'
alias gfbs='git flow bugfix start'
alias gfbf='git flow bugfix finish'
alias gfbp='git flow bugfix publish'
alias gfbt='git flow bugfix track'
alias gfbd='git flow bugfix diff'
alias gfbr='git flow bugfix rebase'
alias gfbc='git flow bugfix pull'
alias gfbd='git flow bugfix delete'

#for linux
alias apt='apt-get install '
alias s='sudo'
eval "$(fasd --init auto)"

#别名设置举例
# export PATH="/usr/local/sbin:$PATH"
#export PATH="$HOME/.node/bin:$PATH"
#export PATH="$HOME/.npm-packages/bin:$PATH"

#alias for cnpm
#alias cnpm="npm --registry=https://registry.npm.taobao.org \
    #--cache=$HOME/.npm/.cache/cnpm \
    #--disturl=https://npm.taobao.org/dist \
    #--userconfig=$HOME/.cnpmrc"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Use ag instead of the default find command for listing candidates.
# - The first argument to the function is the base path to start traversal
# - Note that ag only lists files not directories
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
    ag -g "" "$1"
}
it2prof() { echo -e "\033]50;SetProfile=$1\a" }

# Use ~~ as the trigger sequence instead of the default **
export FZF_COMPLETION_TRIGGER='jj'

# Options to fzf command
export FZF_COMPLETION_OPTS='+c -x'

# Use ag instead of the default find command for listing candidates.
# - The first argument to the function is the base path to start traversal
# - Note that ag only lists files not directories
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
    ag -g "" "$1"
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#

if type complete &>/dev/null; then
    _npm_completion () {
        local words cword
        if type _get_comp_words_by_ref &>/dev/null; then
            _get_comp_words_by_ref -n = -n @ -w words -i cword
        else
            cword="$COMP_CWORD"
            words=("${COMP_WORDS[@]}")
        fi

        local si="$IFS"
        IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
            COMP_LINE="$COMP_LINE" \
            COMP_POINT="$COMP_POINT" \
            npm completion -- "${words[@]}" \
            2>/dev/null)) || return $?
        IFS="$si"
    }
    complete -o default -F _npm_completion npm
elif type compdef &>/dev/null; then
    _npm_completion() {
        local si=$IFS
        compadd -- $(COMP_CWORD=$((CURRENT-1)) \
            COMP_LINE=$BUFFER \
            COMP_POINT=0 \
            npm completion -- "${words[@]}" \
            2>/dev/null)
        IFS=$si
    }
    compdef _npm_completion npm
elif type compctl &>/dev/null; then
    _npm_completion () {
        local cword line point words si
        read -Ac words
        read -cn cword
        let cword-=1
        read -l line
        read -ln point
        si="$IFS"
        IFS=$'\n' reply=($(COMP_CWORD="$cword" \
            COMP_LINE="$line" \
            COMP_POINT="$point" \
            npm completion -- "${words[@]}" \
            2>/dev/null)) || return $?
        IFS="$si"
    }
    compctl -K _npm_completion npm
fi
###-end-npm-completion-###




# utility function used to write the command in the shell
writecmd() {
  perl -e '$TIOCSTI = 0x5412; $l = <STDIN>; $lc = $ARGV[0] eq "-run" ? "\n" : ""; $l =~ s/\s*$/$lc/; map { ioctl STDOUT, $TIOCSTI, $_; } split "", $l;' -- $1
}

# fkill - kill process
fkill() {
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

# fbr - checkout git branch (including remote branches)
fbr() {
  local branches branch
  branches=$(git branch --all | grep -v HEAD) &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

# fco - checkout git branch/tag
fco() {
  local tags branches target
  tags=$(
    git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
    git branch --all | grep -v HEAD             |
    sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
    sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
    (echo "$tags"; echo "$branches") |
    fzf-tmux -l40 -- --no-hscroll --ansi +m -d "\t" -n 2 -1 -q "$*") || return
  git checkout $(echo "$target" | awk '{print $2}')
}

# fcoc - checkout git commit
fcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# fcs - get git commit sha
# example usage: git rebase -i `fcs`
fcs() {
  local commits commit
  commits=$(git log --color=always --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e --ansi --reverse) &&
  echo -n $(echo "$commit" | sed "s/ .*//")
}

# fstash - easier way to deal with stashes
# type fstash to get a list of your stashes
# enter shows you the contents of the stash
# ctrl-d shows a diff of the stash against your current HEAD
# ctrl-b checks the stash out as a branch, for easier merging
fstash() {
  local out q k sha
  while out=$(
    git stash list --pretty="%C(yellow)%h %>(14)%Cgreen%cr %C(blue)%gs" |
    fzf --ansi --no-sort --query="$q" --print-query \
        --expect=ctrl-d,ctrl-b);
  do
    mapfile -t out <<< "$out"
    q="${out[0]}"
    k="${out[1]}"
    sha="${out[-1]}"
    sha="${sha%% *}"
    [[ -z "$sha" ]] && continue
    if [[ "$k" == 'ctrl-d' ]]; then
      git diff $sha
    elif [[ "$k" == 'ctrl-b' ]]; then
      git stash branch "stash-$sha" $sha
      break;
    else
      git stash show -p $sha
    fi
  done
}

# v - open files in ~/.viminfo
vinfo() {
  local files
  files=$(grep '^>' ~/.viminfo | cut -c3- |
          while read line; do
            [ -f "${line/\~/$HOME}" ] && echo "$line"
          done | fzf-tmux -d -m -q "$*" -1) && vim ${files//\~/$HOME}
}

export PATH="$HOME/.yarn/bin:$PATH"
source "$HOME/.vim/plugged/gruvbox/gruvbox_256palette.sh"
#neoterm
if [ -n "${NVIM_LISTEN_ADDRESS+x}" ]; then
  alias nh='nvr -o'
  alias nv='nvr -O'
  alias t='nvr --remote-tab'
fi
