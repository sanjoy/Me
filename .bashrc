# .bashrc
# Author Sanjoy Das <sanjoy@playingwithpointers.com>

# Enable Vi keybindings
set -o vi

# Set the terminal prompts to something a little less obstrusive
PS1='${debian_chroot:+($debian_chroot)}\w \$ '
PS2=' ... '

# Make grep always show the line numbers
alias grep='grep -n'

# Make cp and mv prompt before overwriting.
alias cp='cp -i'
alias mv='mv -i'

# Better readability
alias du='du -kh'
alias df='df -kTh'

# Special alias to show all files
alias la='ls -a'

# Pardon minor spelling errors
shopt -s cdspell

# Something I use a lot for exploring large codebases
function find_symbol {
	if [ -n "$1" -a -n "$2" ]; then
		find . -name "*.$1" | xargs grep -n --mmap "$2"
	else
		echo "Usage: find_symbol <file-extension> <symbol>"
	fi
}

# Changes the wallpaper to a randomly selected one.
function wallpaper {
	feh --bg-scale "$(find ~/.wallpapers -name "*jpg" | shuf -n1)"
}

# Picks up a random video from the current directory tree and plays it using mplayer
function entertain {
	mplayer "`find . -name '*avi' -or -name '*mp4' -or -name '*mpg' \
	-or -name '*mpeg' -or -name '*mkv' -or -name '*flv' -or -name \
	'*divx' -or -name '*m2v' | shuf -n1`"
# Add more -or and -name s to add more extensions later. This should do for now.
}

# Set the http proxy.
export http_proxy=`cat ~/.http_proxy`

echo "   The current time is `date`. The system has been up for $(uptime | awk '{print $3}') days."

