# .zshrc
# Author Sanjoy Das <sanjoy@playingwithpointers.com>

# Set the terminal prompts to something a little less obstrusive
PS1="$(print '%{\e[1;32m%} %B%~ $ %b%{\e[0m%}')"
PS2="$(print '%{\e[0;32m%} ... %{\e[0m%}')"

EDITOR="emacsclient"

# Make grep always show the line numbers
alias grep='grep -n'

# Open emacsclient with tt
alias tt='emacsclient -n'

# patchd for trying patches
alias patchd='patch --dry-run'

function ttf {
	if [ -n "$@" ]; then
		emacsclient -n `find . -name "$@" | head -n 1`
	else
		echo "Usage ttf <file-name>"
	fi
}

# Make cp and mv prompt before overwriting.
alias cp='cp -i'
alias mv='mv -i'

# Better readability
alias du='du -kh'
alias df='df -kTh'

# Special alias to show all files
alias la='ls -a'

# Something I use a lot for exploring large codebases
function find_symbol {
	if [ -n "$1" -a -n "$2" ]; then
		find . -name "*.$1" | xargs grep -n --mmap "$2"
	else
		echo "Usage: find_symbol <file-extension> <symbol>"
	fi
}

# Another useful function
function find_file {
	if [ -n "$@" ]; then
		find . -name "$@"
	else
		echo "Usage: find_file <file-filter>"
	fi
}

# Changes the wallpaper to a randomly selected one.
function wallpaper {
	feh --bg-scale "$(find ~/.wallpapers/wide -name "*jpg" | shuf -n1)"
}

# Picks up a random video from the current directory tree and plays it using mplayer
function entertain {
	file="`find . -name '*avi' -or -name '*mp4' -or -name '*mpg' \
	-or -name '*mpeg' -or -name '*mkv' -or -name '*flv' -or -name \
	'*divx' -or -name '*m2v' -name '*3gp' -or -name '*wmv' | shuf -n1`"
	echo "Playing $file."
	mplayer -msglevel all=-1 "$file"
# Add more -or and -name s to add more extensions later. This should do for now.
}

# Set the http proxy.
export http_proxy=`cat ~/.http_proxy`

# Alias the two screen sessions I generally have active
alias scr-b='screen -r Background'
alias scr-d='screen -r Downloads'

echo "   The current time is `date`."

# The following lines were added by compinstall
zstyle :compinstall filename '/home/sanjoy/.zshrc'

autoload -Uz compinit
compinit

# End of lines added by compinstall

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install