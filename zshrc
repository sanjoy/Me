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

export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

# Open emacsclient with tt
alias tt='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n'

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
HISTSIZE=10000
SAVEHIST=10000
bindkey -e
# End of lines configured by zsh-newuser-install

# Pretty directory listing
alias ls='ls -G'

# So that I always can use `tt'
export ALTERNATE_EDITOR="nano"

bindkey '^H' backward-delete-word
bindkey '^S' push-line

setopt APPEND_HISTORY

export CVS_RSH=ssh

# this treats each component of a path as a word
export WORDCHARS=''

export ACK_PAGER='less -FRXS'

alias less='less -FRXS'

WORK_CONFIG=~sanjoy/.work-zshrc

if [[ -f $WORK_CONFIG ]]; then
   . $WORK_CONFIG
else
    # Brew needs this
    export PATH="/Users/sanjoy/Library/Haskell/bin:/Users/sanjoy/prefix/clang/bin:/Users/sanjoy/prefix/bin:/Users/sanjoy/prefix/arcanist/arcanist/bin:/usr/local/bin:${PATH}"
fi

if [[ `hostname` == "bolt" ]]; then
    function ack {
	git rev-parse --git-dir &> /dev/null
	if [[ $? == "0" ]]; then
	    git grep $@
	    return
	fi
	/usr/local/bin/ack $1
    }
else
    function pg {
	p4 grep -s -e "$1" "`p4 dirs .`/..." | less -FRXS
    }
    function pg-full {
	find . -maxdepth 3 -type d  | while read d; do
	    p4 grep -s -e "$1" "`p4 dirs $d`/..."
	done 2>&1 | grep -v "no file(s) of type text" | \
	    less -FRXS
    }


    function ack {
	git rev-parse --git-dir &> /dev/null
	if [[ $? == "0" ]]; then
	    git grep $@
	    return
	fi

	p4 where &> /dev/null
	if [[ $? == "0" ]]; then
	    pg $@
	    return
	fi

	/usr/local/bin/ack $1
    }
fi

DIRSTACKSIZE=100
setopt autopushd

function tad {
  tmux a -d
}

setopt histignorespace

DIRSTACKSIZE=1000
setopt autopushd

. ~/.github-token

# OPAM configuration
. /Users/sanjoy/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
