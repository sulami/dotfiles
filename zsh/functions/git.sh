git_add_remote()
{
    if [ "$#" -ne 2 ]
    then
        echo "Usage: $0 <name> <location>" >&2
        return 1
    fi

    if [[ ! "$2" =~ ^(http|https|git|ssh)://.* ]]
    then
        mkdir -p "$2" && git init --bare "$2"
    elif [[ "$2" =~ ssh://.* ]]
    then
        ssh ${GITSERVER} "mkdir -p /srv/git/${DIR}.git && git init --bare /srv/git/${DIR}.git"
    fi
    git remote add "$1" "$2"
}

git_forward()
{
    DIR="$(basename ${PWD})"
    ssh ${GITSERVER} "cd /srv/git/${DIR}.git/hooks && mv post-update.sample post-update && echo 'git push --tags ssh://git@github.com/sulami/${DIR} master' > post-update"
}

git_init()
{
    git init
    DIR="$(basename ${PWD})"
    git_add_remote origin ${GITURL}/${DIR}.git
    git_add_remote gh https://github.com/sulami/${DIR}.git
}

git_hist()
{
    count=0
    for DIR in $(find -maxdepth 1 -type d); do
        cd $DIR
        let count="count + $(git rev-list --count --since=$1 master)"
        cd ..
    done
    echo "Commits in ${1}: $count"
}

