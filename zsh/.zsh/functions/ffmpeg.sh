# This whole block here is used to convert flacs and mp3s to opus.
opusconvert()
{
    newfile="$(echo "$1" | rev | cut -d. -f2- | rev).opus"
    ffmpeg -i "$1" -acodec opus -b:a 128k -hide_banner "$newfile" < /dev/null
}

flac2opus()
{
    find . -name '*.flac' -print0 | while IFS= read -r -d '' file; do
        opusconvert "$file"
    done
}

mp32opus()
{
    find . -name '*.mp3' -print0 | while IFS= read -r -d '' file; do
        opusconvert "$file"
    done
}

