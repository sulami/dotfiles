RES="1920x1080"
FPS="30"
QUAL="medium"
STREAM_KEY="live_27370333_oiBEe7bfi71fz3wIr27CRXpmszQts5"

# -af aresample \

ffmpeg \
-f x11grab \
-video_size hd1080 \
-framerate $FPS \
-i :0.0+0,0 \
-f pulse \
-i default \
-vcodec libx264 \
-preset "$QUAL" \
-s $RES \
-filter_complex amix=inputs=1:duration=first:dropout_transition=3 \
-acodec libmp3lame \
-ar 44100 \
-threads 8 \
-b:a 512k \
-pix_fmt yuv420p \
-f flv "rtmp://live.justin.tv/app/$STREAM_KEY"

