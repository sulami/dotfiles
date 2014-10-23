git_add_remote()
{
    mkdir -p $2 && git init --bare $2
    git remote add $1 $2
}

