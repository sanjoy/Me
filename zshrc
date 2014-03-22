# .zshrc
# Author Sanjoy Das <sanjoy@playingwithpointers.com>

# Set the terminal prompts to something a little less obstrusive
function prompt_command {
  PS1="$(print -n "\n{ %{\e[35m%}%m \e[0m}"; print '%{\e[1;30m%} %B%~ \n %C $ %b%{\e[0m%}')"
}

typeset -a precmd_functions
precmd_functions+=prompt_command

PS2="$(print '%{\e[0;30m%} ... %{\e[0m%}')"

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
[[ $TERM == "dumb" ]] && unsetopt zle && PS2='> '

export EDITOR="emacsclient"

# Make grep always show the line numbers
alias grep='grep -n --color'

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
		find . -name "*.$1" | xargs grep -n --mmap -- "$2" | less -FRSX
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
		find . -iregex "$@" | less -FRSX
	else
		echo "Usage: find-file-full [ regex ]"
	fi
}

# I use this one most of the time
function find-file {
	if [ -n "$@" ]; then
		x-find-file ".*$@.*" | less -FRSX
	else
		echo "Usage: find-file-full [ regex ]"
	fi
}

MEDIA_PLAYER='/usr/bin/vlc'

# Picks up a random video from the current directory tree and plays it using mplayer
function entertain {
	file="`find . -name '*avi' -or -name '*mp4' -or -name '*mpg' \
	-or -name '*mpeg' -or -name '*mkv' -or -name '*flv' -or -name \
	'*divx' -or -name '*m2v' -name '*3gp' -or -name '*wmv' | shuf -n1`"
	echo "Playing $file."
	"$MEDIA_PLAYER" "$file"
# Add more -or and -name s to add more extensions later. This should do for now.
}

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

# Pretty directory listing
alias ls='ls -G'

# So that I always can use `tt'
export ALTERNATE_EDITOR="nano"

export PATH="/home/sanjoy/prefix/bin:/Users/sanjoy/Library/Haskell/bin/:$PATH"

bindkey '^H' backward-delete-word
bindkey '^S' push-line
alias play="$MEDIA_PLAYER"

setopt share_history
setopt APPEND_HISTORY

export CVS_RSH=ssh

# I like that this treats each component of a path as a word.
export WORDCHARS=''

# Automatically append a / after ..
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}

zle -N rationalise-dot
bindkey . rationalise-dot
export ACK_PAGER='less -FRXS'

alias less='less -FRXS'

function build-etags-list {
   find . -name '*.h' -or -name '*.hh' -or -name '*.hpp' -or     \
          -name '*.c' -or -name '*.cc' -or -name '*.cpp' -type f \
   | xargs etags -f TAGS
}

function ack-c {
    ack --color --type=cc "$@" | less -FRXS
}

function ack-h {
    ack --color --type=hh "$@" | less -FRXS
}

function ack-cc {
    ack --color --type=cpp "$@" | less -FRXS
}

AZUL_CONFIG=/home/sanjoy/azul-zshrc

if [[ -f $AZUL_CONFIG ]]; then
   . $AZUL_CONFIG
fi

# Brew needs this
PATH="/usr/local/bin:$PATH"

alias gg='git grep'

function pg {
  p4 grep -e "$1" "`p4 dirs .`/..." | less -FRXS
}

function ack {
  git rev-parse --git-dir &> /dev/null
  if [[ $? == "0" ]]; then
    # git repo
    gg $1
    return
  fi

  p4 where &> /dev/null
  if [[ $? == "0" ]]; then
    # p4 repo
    pg $1
    return
  fi

  ~sanjoy/prefix/bin/ack-grep $1
}
