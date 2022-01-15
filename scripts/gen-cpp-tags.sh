#!/bin/bash

if [[ $# == 0 ]]; then
    echo "Usage: $0 [directories]"
    exit 1
fi

find $@ -name '*.h' \
     -or -name '*.cpp' \
     -or -name '*.c' \
     -or -name '*.cc' \
     -or -name '*.hpp' \
     -or -name '*.hh' \
    | xargs ctags -a -e
