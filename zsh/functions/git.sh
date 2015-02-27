#!/bin/zsh

git_add_remote()
{
    if [ "$#" -ne 2 ]
    then
        echo "Usage: $0 <name> <location>" >&2
        return 1
    fi

    if [[ ! "$2" =~ ^(http|https|git)://.* ]]
    then
        mkdir -p "$2" && git init --bare "$2"
    fi
    git remote add "$1" "$2"
}

