SIZE="hd1080"
RESOLUTION="1280x720"
FRAMERATE="30"
STREAMKEY="live_27370333_oiBEe7bfi71fz3wIr27CRXpmszQts5"
THREADS="8"
BITRATE="2048"
AUDIO="pulse"
CHANNELS="2"
VERBOSE=false

while getopts ":s:r:f:k:t:a:c:vh" opt; do
    case $opt in
        s)
            SIZE=$OPTARG
            ;;
        r)
            RESOLUTION=$OPTARG
            ;;
        f)
            FRAMERATE=$OPTARG
            ;;
        k)
            STREAMKEY=$OPTARG
            ;;
        t)
            THREADS=$OPTARG
            ;;
        b)
            BITRATE=$OPTARG
            ;;
        a)
            AUDIO=$OPTARG
            ;;
        c)
            CHANNELS=$OPTARG
            ;;
        v)
            VERBOSE=true
            ;;
        h)
            echo "Options:"
            echo "  -s <str>        - Desktop size (def: hd1080)"
            echo "  -r <int>x<int>  - Stream resolution (def: 1280x720)"
            echo "  -f <int>        - Framerate (def: 30)"
            echo "  -k <str>        - Streamkey (Acquire from Twitch.tv)"
            echo "  -t <int>        - Threads (def: 8)"
            echo "  -b <int>        - Video bitrate in kb/s (def: 2048)"
            echo "  -a <str>        - Audio framework (def: pulse)"
            echo "  -c <int>        - Audio Channels (def: 2)"
            echo "  -v              - Verbose output"
            echo "  -h              - This help output"
            exit 0
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires and argument" >&2
            exit 1
            ;;
    esac
done

for c in `seq ${CHANNELS}`; do
    AUDIOCODE="$AUDIOCODE -f $AUDIO -i default"
done

ffmpeg \
-f x11grab \
-video_size ${SIZE} \
-framerate ${FRAMERATE} \
-i :0.0+0,0 \
$AUDIOCODE \
-vcodec libx264 \
-preset medium \
-s ${RESOLUTION} \
-b:v ${BITRATE}k \
-minrate ${BITRATE}k \
-maxrate ${BITRATE}k \
-bufsize $((2 * ${BITRATE}))k \
-g $((2 * ${FRAMERATE})) \
-filter_complex amix=inputs=${CHANNELS}:duration=first:dropout_transition=3 \
-acodec libmp3lame \
-ar 44100 \
-b:a 128k \
-threads ${THREADS} \
-pix_fmt yuv420p \
-f flv rtmp://live.justin.tv/app/${STREAMKEY}

