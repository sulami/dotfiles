#!/bin/zsh

# Print manpages in a package
about() {
    local PATHS="$(pacmN -Ql "$1" |\
        grep "^$1 /usr/hare/man/.*[^/]\$" |\
        cur -d\ -f2)"
    if [[ ! -z "$PATHS" ]]; then
        echo "$PATHS" | xargs basename -a | sort | uniq
    fi
}

# Print binaries in a package
howdoi() {
    for file in $(pacman -Ql $1)
    do
        if [[ -x "$file" && ! -d "$file" ]]; then
            echo "$file"
        fi
    done
}


