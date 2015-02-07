RESOLUTION="1280x720"
FRAMERATE="30"
STREAMKEY="live_27370333_oiBEe7bfi71fz3wIr27CRXpmszQts5"
THREADS="8"
BITRATE="2048"
AUDIO="pulse"
CHANNELS="2"
VERBOSE=false

while getopts ":r:f:k:t:a:c:v" opt; do
    case $opt in
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
-video_size hd1080 \
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

