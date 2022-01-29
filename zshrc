# .zshrc
# Author Sanjoy Das <sanjoy@playingwithpointers.com>

PROMPT='%2~ $ '

export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

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

# So that I always can use `tt'
export ALTERNATE_EDITOR="nano"

bindkey '^H' backward-delete-word
bindkey '^S' push-line

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
    export PATH="${HOME}/Library/Haskell/bin:${HOME}/prefix/clang/bin:${HOME}/prefix/bin:${HOME}/Code/arcanist/bin:/usr/local/bin:${PATH}"
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

	grep -r $1 .
    }
fi

function tad {
  tmux a -d
}

if [[ -f ~/.github-token ]]; then
    . ~/.github-token
fi

# OPAM configuration
. ${HOME}/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

export P4CONFIG='.p4config'
export PATH="$HOME/.gem/ruby/2.6.0/bin:$PATH"
source $HOME/.cargo/env

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
export PATH="/usr/local/opt/ruby/bin:/usr/local/lib/ruby/gems/3.0.0/bin:$PATH"
export PATH="${HOME}/Code/Me/scripts:$PATH"




# Shell configuration

unsetopt EXTENDED_GLOB # To allow ^ in git cmd lines

setopt APPEND_HISTORY # Concurrent ZSH sessions append to history file
setopt HIST_IGNORE_SPACE  # Commands with leading space are not remembered

DIRSTACKSIZE=1000
setopt AUTO_PUSHD # Always allow popd

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/sanjoy/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/sanjoy/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/sanjoy/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/sanjoy/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

