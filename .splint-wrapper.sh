#!/bin/zsh

splint_args=""
for arg in "$@"; do
	if [[ $arg =~ '(.*\.c)|(-I.*)' ]]; then
		splint_args="$arg $splint_args"
	fi
done
if [[ -n $splint_args ]]; then
	$splint_args="$splint_args"
	splint $=splint_args &>> /tmp/splint-output
fi
cc $@
