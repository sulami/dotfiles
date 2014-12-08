#!/bin/zsh

git_add_remote()
{
    if [[ ! "$2" =~ ^(http|https|git)://.* ]]
    then
        mkdir -p "$2" && git init --bare "$2"
    fi
    git remote add "$1" "$2"
}

