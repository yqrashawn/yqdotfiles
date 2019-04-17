# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/Cellar/fzf/0.17.5/bin* ]]; then
  export PATH="$PATH:/usr/local/Cellar/fzf/0.17.5/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/usr/local/opt/fzf/shell/key-bindings.zsh"

