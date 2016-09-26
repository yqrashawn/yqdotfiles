# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/Rashawn/.fzf/bin* ]]; then
  export PATH="$PATH:/Users/Rashawn/.fzf/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" == */Users/Rashawn/.fzf/man* && -d "/Users/Rashawn/.fzf/man" ]]; then
  export MANPATH="$MANPATH:/Users/Rashawn/.fzf/man"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/Rashawn/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/Users/Rashawn/.fzf/shell/key-bindings.bash"

