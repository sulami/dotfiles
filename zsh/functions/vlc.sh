ytpl2vlc()
{
    youtube-dl -g $1 | xargs vlc
}

