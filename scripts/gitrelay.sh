#!/bin/sh

# Push all repos in the current dir to their respective local remote

for DIR in $(find . -maxdepth 1 -type d -name "*.git")
do
    cd $DIR
    git push local master --tags
    cd ..
done

