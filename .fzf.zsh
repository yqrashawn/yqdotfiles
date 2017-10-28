# Setup fzf
# ---------
# if [[ ! "$PATH" == */usr/local/Cellar/fzf/0.17.1/bin* ]]; then
#   export PATH="$PATH:/usr/local/Cellar/fzf/0.17.1/bin"
# fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/usr/local/Cellar/fzf/0.17.1/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/usr/local/Cellar/fzf/0.17.1/shell/key-bindings.zsh"

