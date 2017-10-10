# Setup fzf
# ---------
if [[ ! "$PATH" == *${HOME}.fzf/bin* ]]; then
  export PATH="$PATH:${HOME}.fzf/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" == *${HOME}.fzf/man* && -d "${HOME}.fzf/man" ]]; then
  export MANPATH="$MANPATH:${HOME}.fzf/man"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "${HOME}.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# bindkey '^E' jump
# ------------

# source "~/.fzf/shell/fzf-git.zsh"
source "${HOME}/.fzf/shell/marks.zsh"
source "${HOME}/.fzf/shell/key-bindings.zsh"

