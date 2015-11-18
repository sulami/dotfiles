# Open a website in vim
cv()
{
    curl "$1" | pv | vim -
}

# Start an asynchronous test-runner
test_runner()
{
    if [ ! -p commands.fifo ]; then
        mkfifo commands.fifo
    fi

    while true; do
        sh -c "$(cat commands.fifo)"
    done
}

