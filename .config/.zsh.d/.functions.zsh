# Create a new directory and enter it
function md() {
	  mkdir -p "$@" && cd "$@"
}

# git commit browser. needs fzf
glog() {
    git log --graph --color=always \
        --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
        fzf --ansi --no-sort --reverse --tiebreak=index --toggle-sort=\` \
            --bind "ctrl-m:execute:
                echo '{}' | grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R'"
}

# get gzipped size
function gz() {
	  echo "orig size    (bytes): "
	  cat "$1" | wc -c
	  echo "gzipped size (bytes): "
	  gzip -c "$1" | wc -c
}

function extract() {
	if [ -f "$1" ] ; then
		local filename=$(basename "$1")
		local foldername="${filename%%.*}"
		local fullpath=`perl -e 'use Cwd "abs_path";print abs_path(shift)' "$1"`
		local didfolderexist=false
		if [ -d "$foldername" ]; then
			didfolderexist=true
			read -p "$foldername already exists, do you want to overwrite it? (y/n) " -n 1
			echo
			if [[ $REPLY =~ ^[Nn]$ ]]; then
				return
			fi
		fi
		mkdir -p "$foldername" && cd "$foldername"
		case $1 in
			*.tar.bz2) tar xjf "$fullpath" ;;
			*.tar.gz) tar xzf "$fullpath" ;;
			*.tar.xz) tar Jxvf "$fullpath" ;;
			*.tar.Z) tar xzf "$fullpath" ;;
			*.tar) tar xf "$fullpath" ;;
			*.taz) tar xzf "$fullpath" ;;
			*.tb2) tar xjf "$fullpath" ;;
			*.tbz) tar xjf "$fullpath" ;;
			*.tbz2) tar xjf "$fullpath" ;;
			*.tgz) tar xzf "$fullpath" ;;
			*.txz) tar Jxvf "$fullpath" ;;
			*.zip) unzip "$fullpath" ;;
			*) echo "'$1' cannot be extracted via extract()" && cd .. && ! $didfolderexist && rm -r "$foldername" ;;
		esac
	else
		echo "'$1' is not a valid file"
	fi
}

camerausedby() {
	  echo "Checking to see who is using the iSight camera… 📷"
	  usedby=$(lsof | grep -w "AppleCamera\|USBVDC\|iSight" | awk '{printf $2"\n"}' | xargs ps)
	  echo -e "Recent camera uses:\n$usedby"
}

# animated gifs from any video
# from alex sexton   gist.github.com/SlexAxton/4989674
gifify() {
    if [[ -n "$1" ]]; then
	      if [[ $2 == '--good' ]]; then
	          ffmpeg -i "$1" -r 10 -vcodec png out-static-%05d.png
	          time convert -verbose +dither -layers Optimize -resize 900x900\> out-static*.png  GIF:- | gifsicle --colors 128 --delay=5 --loop --optimize=3 --multifile - > "$1.gif"
	          rm out-static*.png
	      else
	          ffmpeg -i "$1" -s 600x400 -pix_fmt rgb24 -r 10 -f gif - | gifsicle --optimize=3 --delay=3 > "$1.gif"
	      fi
    else
	      echo "proper usage: gifify <input_movie.mov>. You DO need to include extension."
    fi
}

# turn that video into webm.
# brew reinstall ffmpeg --with-libvpx
webmify(){
	  ffmpeg -i "$1" -vcodec libvpx -acodec libvorbis -isync -copyts -aq 80 -threads 3 -qmax 30 -y "$2" "$1.webm"
}

# direct it all to /dev/null
function nullify() {
    "$@" >/dev/null 2>&1
}

# Copy w/ progress
rscp () {
    rsync -WavP --human-readable --progress $1 $2
}

DESKTOP_NAMES=("officeimac" "homeimac")
MOBILE_NAMES=("macbook")

function is-osx {
    [[ "$(uname)" == "Darwin" ]]
}

function is-linux {
    [[ "$(uname)" == "Linux" ]]
}

function is-desktop {
    loc=$(scutil --get ComputerName || hostname)
    for i in "${DESKTOP_NAMES[@]}"; do
        ([[ "$loc" == "$i" ]] || [[ "$loc" == "$i".local ]]) && return 0
    done
    return 1
}

function is-mobile {
    loc=$(scutil --get ComputerName || hostname)
    for i in "${MOBILE_NAMES[@]}"; do
        ([[ "$loc" == "$i" ]] || [[ "$loc" == "$i".local ]]) && return 0
    done
    return 1
}

function cheat() {
    curl cht.sh/$1
}

# Pulled from https://stackoverflow.com/questions/18641864/git-bash-shell-fails-to-create-symbolic-links
# https://stackoverflow.com/users/124119/camilo-martin

# Detect windows (assumes we are in 'msysgit' or similar).
windows() { [[ -n "$WINDIR" ]]; }

# Cross-platform symlink function.
#  With one parameter, it will check whether the parameter is a symlink.
#  With two parameters, it will create a symlink to a file or directory,
#   with syntax: link $linkname $target
link() {
    if [[ -z "$2" ]]; then
        # Link-checking mode.
        if windows; then
            fsutil reparsepoint query "$1" > /dev/null
        else
            [[ -h "$1" ]]
        fi
    else
        # Link-creation mode.
        if windows; then
            # Windows needs to be told if it's a directory or not. Infer that.
            # Also: note that we convert `/` to `\`. In this case it's necessary.
            if [[ -d "$2" ]]; then
                cmd <<< "mklink /D \"$1\" \"${2//\//\\}\"" > /dev/null
            else
                cmd <<< "mklink \"$1\" \"${2//\//\\}\"" > /dev/null
            fi
        else
            # You know what? I think ln's parameters are backwards.
            ln -s "$2" "$1"
        fi
    fi
}

# Pulled from https://stackoverflow.com/questions/18641864/git-bash-shell-fails-to-create-symbolic-links
# https://stackoverflow.com/users/124119/camilo-martin

# Detect windows (assumes we are in 'msysgit' or similar).
windows() { [[ -n "$WINDIR" ]]; }

# Remove a link, cross-platform.
rmlink() {
  if windows; then
    # Again, Windows needs to be told if it's a file or directory.
    if [[ -d "$1" ]]; then
      rmdir "$1";
    else
      rm "$1"
    fi
  else
    rm "$1"
  fi
}