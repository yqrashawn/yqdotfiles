if [ -n "$INSIDE_EMACS" ]; then
    chpwd() { print -P "\033AnSiTc %d" }
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
fi

# function environment_variable_exists {
#     eval "value=\"\${$1+x}\""
#     [ ! -z $value ]
# }

# function emacs_ansi_term_support {
#     echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
#     echo -e "\033AnSiTc" "$(pwd)"
#     if [ $(uname) = "SunOS" ]; then
#         # The -f option does something else on SunOS and is not needed anyway.
#         hostname_options="";
#     else
#         hostname_options="-f";
#     fi
#     # XXX/TODO: This disables correct setting of the current directory
#     # when in an sshed shell when inside of emacs
#     # echo -e "\033AnSiTh" "$(hostname $hostname_options)" # Using the -f option can #
#     # cause problems on some OSes.
# }

# if environment_variable_exists INSIDE_EMACS; then
#     if [[ $INSIDE_EMACS == *"term"* ]]
#     then
#         add-zsh-hook precmd emacs_ansi_term_support
#     fi
# fi

# Highlight section titles in manual pages.
export LESS_TERMCAP_md="${yellow}";

# Don’t clear the screen after quitting a manual page.
export MANPAGER='less -X';

# Make Python use UTF-8 encoding for output to stdin, stdout, and stderr.
export PYTHONIOENCODING='UTF-8';

# Enable persistent REPL history for `node`.
export NODE_REPL_HISTORY=~/.node_history;
# Allow 32³ entries; the default is 1000.
export NODE_REPL_HISTORY_SIZE='32768';
# Use sloppy mode by default, matching web browsers.
export NODE_REPL_MODE='sloppy';
# 10ms for key sequences
KEYTIMEOUT=1

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
    colorflag="--color"
    export LS_COLORS='no=00:fi=00:di=01;31:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'
else # macOS `ls`
    colorflag="-G"
    export LSCOLORS='BxBxhxDxfxhxhxhxhxcxcx'
fi


# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
    alias "$method"="lwp-request -m '$method'"
done

#neoterm
if [ -n "${NVIM_LISTEN_ADDRESS+x}" ]; then
    alias nh='nvr -o'
    alias nv='nvr -O'
    alias t='nvr --remote-tab'
fi

# source "${ZDOTDIR:-$HOME}/.zprezto/modules/gitsome/init.sh"

# https://github.com/b-ryan/powerline-shell
function powerline_precmd() {
    PS1="$(powerline-shell --shell zsh $?)"
}

function install_powerline_precmd() {
    for s in "${precmd_functions[@]}"; do
        if [ "$s" = "powerline_precmd" ]; then
            return
        fi
    done
    precmd_functions+=(powerline_precmd)
}

if [ "$TERM" != "linux" ]; then
    install_powerline_precmd
fi

: ${TERM_TITLE_SET_MULTIPLEXER:=1}

# function term_set_title() {
# 	emulate -L zsh
# 	local term_is_known=0 term_is_multi=0
# 	if [[ \
# 		$TERM == rxvt-unicode*
# 		|| $TERM == xterm*
# 		|| ! -z $TMUX
# 	]] then
# 		term_is_known=1
# 	fi
# 	if [[ ! -z $TMUX ]] then
# 		term_is_multi=1
# 	fi
# 	if [[ $term_is_known -ne 1 ]] then
# 		return
# 	fi
# 	printf '\033]0;%s\007' ${1//[^[:print:]]/}
# 	if [[ \
# 		$TERM_TITLE_SET_MULTIPLEXER -eq 1
# 		&& $term_is_multi -eq 1
# 	]] then
# 		printf '\033k%s\033\\' ${1//[^[:print:]]/}
# 	fi
# }

# function term_title_get_command() {
# 	emulate -L zsh
# 	local job_text job_key
# 	typeset -g RETURN_COMMAND
# 	RETURN_COMMAND=$1
# 	# Since ~4.3.5, patch:
# 	# "users/11818: allow non-numeric keys for job status parameters"
# 	# it is possible to use the `fg ...` or `%...` description as a key
# 	# in $jobtexts.
# 	case $1 in
# 		%*) job_key=$1 ;;
# 		fg) job_key=%+ ;;
# 		fg*) job_key=${(Q)${(z)1}[2,-1]} ;;
# 		*) job_key='' ;;
# 	esac
# 	if [[ -z $job_key ]] then
# 		# not a "job to foreground" command - use it as is
# 		return
# 	fi
# 	job_text=${jobtexts[$job_key]} 2> /dev/null
# 	if [[ -z $job_text ]] then
# 		# job lookup failed - use the original command
# 		return
# 	fi
# 	RETURN_COMMAND=$job_text
# }

# function term_title_precmd() {
# 	emulate -L zsh
# 	local cmd='zsh'
# 	local dir='%~'
# 	term_set_title $cmd:${(%)dir}
# }

# function term_title_preexec() {
# 	emulate -L zsh
# 	term_title_get_command $1
# 	local cmd=$RETURN_COMMAND
# 	local dir='%~'
# 	term_set_title $cmd:${(%)dir}
# }

# autoload -Uz add-zsh-hook
# add-zsh-hook precmd term_title_precmd
# add-zsh-hook preexec term_title_preexec