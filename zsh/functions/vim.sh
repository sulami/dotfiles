# Open a website in vim
cv()
{
    curl "$1" | pv | vim -
}

# Start an asynchronous test-runner
test_runner()
{
    if [ ! -p test-commands ]; then
        mkfifo test-commands
    fi

    while true; do
        sh -c "$(cat test-commands)"
    done
}

