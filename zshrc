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

# Make cp and mv prompt before overwriting.
alias cp='cp -i'
alias mv='mv -i'

# Better readability
alias du='du -kh'
alias df='df -kTh'

# Special alias to show all files
alias la='ls -a'

# Something I use a lot for exploring large codebases
function g-find-symbol {
	if [ -n "$1" -a -n "$2" ]; then
          git grep "$2" -- "*.$1"
	else
		echo "Usage: gfind-symbol [ file-extension ] [ symbol ]"
	fi
}

# Something I use a lot for exploring large codebases
function x-find-symbol {
	if [ -n "$1" -a -n "$2" ]; then
		find . -name "*.$1" | xargs grep -n --mmap -- "$2" | less
	else
		echo "Usage: find-symbol [ file-extension ] [ symbol ]"
	fi
}

function find-symbol {
    git status > /dev/null 2> /dev/null
    if [[ "$?" -eq "0" ]]; then
       g-find-symbol $@
    else
        x-find-symbol $@
    fi
}

# Find files which match the provided regular expression
function x-find-file {
	if [ -n "$@" ]; then
		find . -iregex "$@"
	else
		echo "Usage: find-file-full [ regex ]"
	fi
}

# I use this one most of the time
function find-file {
	if [ -n "$@" ]; then
		x-find-file ".*$@.*"
	else
		echo "Usage: find-file-full [ regex ]"
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
export all_proxy=`cat ~/.all_proxy`

function screen-create () {
	name=$1
	screen -list | grep "$name"
	if [ "$?" -ne "0" ]; then # Background session not yet started
		screen -S "$name"
	else
		screen -r "$name"
	fi
}

alias scr-b='screen-create Background'

# configure zsh's autocompletion system; man zshcompsys

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' matcher-list '+' '+m:{[:lower:]}={[:upper:]}' '+l:|=* r:|=*' '+r:|[._-]=** r:|=**'
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' list-suffixes true

zstyle ':completion:*' group-name ''
zstyle ':completion:*' format '%B%d:%b'
zstyle ':completion:*' verbose true
zstyle ':completion:*' file-sort access
zstyle ':completion:*' list-colors no=00 fi=00 di=01\;34 pi=33 so=01\;35 bd=00\;35 cd=00\;34 or=00\;41 mi=00\;45 ex=01\;32
zstyle ':completion:*' menu 'select=0'
zstyle ':completion:*' list-prompt ''
zstyle ':completion:*' select-prompt ''

zstyle ':completion:*' insert-tab false
zstyle ':completion:*' prompt ''\''%e'\'''
zstyle ':completion:*:manuals' separate-sections true

autoload -Uz compinit
compinit

# End of lines added by compinstall

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install

# Sticky-change directory
function cdm () {
    local tmp
    if [[ -z "${TMUX}" ]]; then
        echo 'fatal: Not inside tmux.'
        return 1
    fi
    if [[ -n "$1" ]]; then
       tmp="$1"
    else
        tmp="${HOME}"
    fi
    cd "${tmp}"
    tmp="${PWD}"
    tmux "set-option" "default-path" "${tmp}"
    return 0
}

function calc () {
	x="print str($@);"
	python -c $x
}

export mysql='mysql --sigint-ignore'

# Proxychains wrappers on common commands
# This sets up the completion properly

compdef xgit=git
compdef xsvn=svn
compdef xssh=ssh
compdef xcvs=cvs

alias xcvs='proxychains cvs'

# Pretty directory listing
alias ls='ls --color=auto'

# So that I always can use `tt'
export ALTERNATE_EDITOR="nano"

export PATH="$PATH:/home/sanjoy/prefix/bin"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/home/sanjoy/prefix/lib"

bindkey '^H' backward-delete-word
