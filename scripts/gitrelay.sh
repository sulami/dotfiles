#!/bin/sh

# Push all repos in the current dir to their respective local remote

for DIR in $(ls)
do
    pushd $DIR
    git push local master --tags
    popd
done

