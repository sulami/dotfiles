# Use youtube-dl to get a SSL-free version of a youtube audio stream and
# add it to the mpd playlist using mpc.
yt2mpd()
{
    youtube-dl --prefer-insecure -f140 -g "$1" | mpc add
}

