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

git_init()
{
    git init
    PWD=$(pwd | sed 's/\//\n/g' | tail -1)
    git_add_remote origin ${HOME}/git/${PWD}.git
    git_add_remote gh https://github.com/sulami/${PWD}
}
