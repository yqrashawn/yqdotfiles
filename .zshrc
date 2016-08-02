#. /usr/local/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh
# oh-my-zsh auto update
DISABLE_UPDATE_PROMPT=true
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# 10ms for key sequences
#KEYTIMEOUT=1
# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="ys"
setopt RM_STAR_WAIT
# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
#plugins=(vim git autojump osx brew zsh-syntax-highlighting colored-man-pages copydir tmux vi-mode web-search git-flow-completion)
plugins=(jump pj fasd vim git osx git-flow-completion zsh-syntax-highlighting colored-man-pages tmux vi-mode)

#for pj
PROJECT_PATHS=(~/workspace ~/workspace/projects ~/workspace/XiheMap ~/workspace/gogs-webhook ~/workspace/projecs/cesium)
plugins+=(zsh-completions)
autoload -U compinit && compinit

# Load zsh-syntax-highlighting.
#source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Load zsh-autosuggestions.
#source ~/.zsh/zsh-autosuggestions/autosuggestions.zsh

# Enable autosuggestions automatically.
zle-line-init() {
    zle autosuggest-start
}
zle -N zle-line-init

#zsh-syntax-highlighting为新加入的 不知道干嘛用的

# User configuration

export PATH=$HOME/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


#zsh自动补全插件 incr-0.2.zsh
source ~/.oh-my-zsh/plugins/incr-0.2.zsh
source ~/.oh-my-zsh/plugins/sublime.plugin.zsh

#安装autojump时添加的代码
#[[ -s ~/.autojump/etc/profile.d/autojump.sh ]] && . ~/.autojump/etc/profile.d/autojump.sh

#for jump
alias k='jump'
alias kk='mark'
alias ks='marks'

alias zc="nvim ~/.zshrc"
alias cl='clear'
alias o='open'
alias lst='ls -t'

alias gitt='git add . && git commit -m '
alias karaconfig='nvim /Users/Rashawn/Library/Application\ Support/Karabiner/private.xml'
alias proxyconfig='nvim /usr/local/etc/privoxy/config'
alias grep='grep -r'
alias ackl='ack -l'
alias sss='proxychains4'
alias rrm='rm *.*~ && rm .*~'
alias lsl='ls -l'
alias pproxy='proxychains4'
alias p='you-get -p mpv '
alias v='nvim'
alias finalz='p http://www.douyutv.com/D3D3D3'
alias vimconfig='nvim /Users/Rashawn/.config/nvim/init.vim'
alias ariaconfig='nvim /Users/Rashawn/.aria2/aria2.conf'
alias ttmux='tmuxinator start haha'
alias tmuxconfig='nvim ~/.tmux.conf'
alias tmux='tmux -3'
alias j='z'
alias jj='zz'
alias tls='tmux ls'
alias surconfig='nvim ~/.surge.conf'
alias cyper='p http://www.douyutv.com/576190'
alias server='http-server'
alias serverp='http-server -p'
alias findh='mdfind -onlyin . '
alias gitshadow="git config --global http.proxy 'socks5://127.0.0.1:1080' \
    git config --global https.proxy 'socks5://127.0.0.1:1080'"
alias gitunshadow="git config --global --unset http.proxy \
    git config -'global --unset https.proxy"
alias banwa='ssh root@45.78.62.230 -p 26812'

alias nd='devtool --watch ./'

#alias cis='sudo nvram boot-args="niog=1"'
#alias ois='sudo nvram -d boot-args'

alias mongodd='mongod -dbpath ~/workspace/projects/mongodb/data'
alias sconfig='source ~/.zshrc'
alias wps='webpack-dev-server --devtool eval'
alias ss='export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152 & '
alias pokego='source bin/activate && python pokecli.py'
alias pokego1='source ~/workspace/projects/PokemonGo-Bot/bin/activate'
alias pokego2='python ~/workspace/projects/PokemonGo-Bot/pokecli.py'
eval "$(thefuck --alias)"

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
alias gs='git status'

#别名设置举例
export PATH="/usr/local/sbin:$PATH"
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
export FZF_COMPLETION_TRIGGER='ii'

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
