ytpl2vlc()
{
    youtube-dl -g $1 | xargs vlc
}

twitch()
{
    livestreamer --player "vlc --file-caching 2000 --network-caching 2000" --hls-segment-threads 3 twitch.tv/$1 best
}

twitch-vod()
{
    livestreamer --player "vlc --file-caching 2000 --network-caching 2000" --hls-segment-threads 3 --player-passthrough=hls "$1" best
}

