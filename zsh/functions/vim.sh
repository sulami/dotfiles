# Open a website in vim
cv()
{
    curl "$1" | pv | vim -
}

# Start an asynchronous listener that executes anything that comes out of
# commands.fifo
async_listener()
{
    if [ ! -p commands.fifo ]; then
        mkfifo commands.fifo
    fi

    while true; do
        sh -c "$(cat commands.fifo)"
    done
}

