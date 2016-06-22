#. /usr/local/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# 10ms for key sequences
#KEYTIMEOUT=1
# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="wedisagree"
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
plugins=(vim git autojump osx zsh-syntax-highlighting colored-man-pages copydir tmux vi-mode git-flow-completion)

#plugins+=(zsh-completions)
autoload -U compinit && compinit

# Load zsh-syntax-highlighting.
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Load zsh-autosuggestions.
source ~/.zsh/zsh-autosuggestions/autosuggestions.zsh

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
[[ -s ~/.autojump/etc/profile.d/autojump.sh ]] && . ~/.autojump/etc/profile.d/autojump.sh


#sublime text 打开各类型文件的设置
alias -s md=st
alias zshconfig="nvim ~/.zshrc"
alias cl='clear'
alias o='open'
alias lst='ls -t'
#alias stwork='st /Users/Rashawn/workspace'
#alias startserver='bundle exec jekyll serve'
alias gitpush='git push -u origin master'
alias gittt='git add . ; git commit -m '
alias karaconfig='nvim /Users/Rashawn/Library/Application\ Support/Karabiner/private.xml'
alias proxyconfig='nvim /usr/local/etc/privoxy/config'
alias grep='grep -r'
alias ackl='ack -l'
alias s='proxychains4'
alias rrm='rm *.*~'

#alias ttask='st /Users/Rashawn/workspace/task/task.TODO'
#alias nnote='st /Users/Rashawn/workspace/notes' 
alias proxy='export http_proxy=socks5://127.0.0.1:1080
            export https_proxy=socks5://127.0.0.1:1080'	
alias ccc='cc -c'
alias lsl='ls -l'
alias pproxy='proxychains4'
alias p='you-get -p mpv '
alias vi='mvim'
# alias vim='/usr/local/Cellar/macvim/7.4-96/MacVim.app/Contents/MacOS/Vim'
alias finalz='p http://www.douyutv.com/D3D3D3'
alias vimconfig='nvim /Users/Rashawn/.vimrc'
alias ariaconfig='nvim /Users/Rashawn/.aria2/aria2.conf'
alias xvimconfig='nvim /Users/Rashawn/.xvimrc'
alias vitemp='vi /Users/Rashawn/Desktop/temp'
alias ttmux='tmuxinator start default'
alias tmuxconfig='nvim ~/.tmux.conf'
alias tmux='tmux -2'
alias tattach='tmux attach -t'
alias tnew='tmux new -s'
alias tls='tmux ls'
alias surconfig='nvim ~/.surge.conf'
alias tkill='tmux kill-session -t'
alias vintageconfig='nvim "/Users/Rashawn/Library/Application Support/Sublime Text 3/Packages/User/.vintageousrc"'
alias cyper='p http://www.douyutv.com/576190'
alias server='http-server'
alias serverp='http-server -p'
alias findh='mdfind -onlyin . '
alias sg='web_search google'
alias gitshadow="git config --global http.proxy 'socks5://127.0.0.1:1080' \
    git config --global https.proxy 'socks5://127.0.0.1:1080'"
alias gitunshadow="git config --global --unset http.proxy \
    git config -'global --unset https.proxy"
alias banwa='ssh -p 26968 root@45.78.62.230'

alias jhbuild='~/.local/bin/jhbuild'

#别名设置举例
#alias g='git'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/sbin:$PATH"
#export PATH="$HOME/.node/bin:$PATH"
export PATH=/usr/local/share/npm/bin:$PATH

#alias for cnpm
alias cnpm="npm --registry=https://registry.npm.taobao.org \
  --cache=$HOME/.npm/.cache/cnpm \
  --disturl=https://npm.taobao.org/dist \
  --userconfig=$HOME/.cnpmrc"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
