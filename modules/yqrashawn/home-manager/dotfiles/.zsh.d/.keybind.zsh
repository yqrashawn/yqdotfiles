# -*- mode: shell-script-*-
# Better history
# Credits to https://coderwall.com/p/jpj_6q/zsh-better-history-searching-with-arrow-keys
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down
bindkey "^P" up-line-or-beginning-search # C-p
bindkey "^N" down-line-or-beginning-search # C-n

# c() {
#     local dir
#     dir="$(fasd -Rdl "$1" | fzy)" && cd "${dir}" || return 1
# }
c() {
    local dir
    dir="$(fasd -Rl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}
zle -N c
# bindkey  c

EmailID="namy.19@gmail.com"

checkgmail() {
	  zle -I
	  (
		    curl -u $EmailID --silent "https://mail.google.com/mail/feed/atom"|
			      tr -d '\n'| awk -F '<entry>' '{for (i=2; i<=NF; i++) {print $i}}'|
			      sed -n "s/<title>\(.*\)<\/title.*name>\(.*\)<\/name>.*/\2 - \1/p"|
			      sed 's/&#34;/"/g'|
			      sed "s/&#38;/&/g"|
			      sed "s/&#39;/'/g"|
			      sed "s/&#60;/</g"|
			      sed "s/&#62;/>/g"|
			      _colorize_mail
	  ) < /dev/tty
}

_colorize_mail() {
    DELIMINATOR=${1:-"-"}
    CA=${2:-"\e[1;34m"}
    CB=${3:-"\e[1;30m"}
	  while read line
	  do
		    var1=$(echo $line | cut -f1 -d$DELIMINATOR)
		    var2=$(echo $line | cut -f2- -d$DELIMINATOR)
		    echo -e "$CA$var1$CB$var2\e[0m"
	  done
}
zle -N checkgmail

fe() {
    local files
    IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
    [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# Modified version where you can press
#   - CTRL-O to open with `open` command,
#   - CTRL-E or Enter key to open with the $EDITOR
fo() {
    local out file key
    IFS=$'\n' out=($(fzf-tmux --query="$1" --exit-0 --expect=ctrl-o,ctrl-e))
    key=$(head -1 <<< "$out")
    file=$(head -2 <<< "$out" | tail -1)
    if [ -n "$file" ]; then
        [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-vim} "$file"
    fi
}

# fuzzy grep open via ag
r() {
    local file

    file="$(rg --no-heading $@ | fzy | awk -F: '{print $1 " +" $2}')"


    if [[ -n $file ]]
    then
        emacsclient $file
    fi
}

zle -N r