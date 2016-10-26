# Setup fzf
# ---------
if [[ ! "$PATH" == */home/yqrashawn/.fzf/bin* ]]; then
  export PATH="$PATH:/home/yqrashawn/.fzf/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" == */home/yqrashawn/.fzf/man* && -d "/home/yqrashawn/.fzf/man" ]]; then
  export MANPATH="$MANPATH:/home/yqrashawn/.fzf/man"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/yqrashawn/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
bindkey '^E' jump
# ------------

# source "/home/yqrashawn/.fzf/shell/fzf-git.zsh"
source "/home/yqrashawn/.fzf/shell/marks.zsh"
source "/home/yqrashawn/.fzf/shell/key-bindings.zsh"

