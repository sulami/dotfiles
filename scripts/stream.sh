RES="1280x720"
FPS="30"
STREAM_KEY="live_27370333_oiBEe7bfi71fz3wIr27CRXpmszQts5"

ffmpeg \
-f x11grab \
-video_size hd1080 \
-framerate $FPS \
-i :0.0+0,0 \
-f pulse \
-i default \
-f pulse \
-i default \
-vcodec libx264 \
-preset medium \
-s $RES \
-b:v 2048k \
-minrate 2048k \
-maxrate 2048k \
-bufsize 4096k \
-g 60 \
-filter_complex amix=inputs=2:duration=first:dropout_transition=3 \
-acodec libmp3lame \
-ar 44100 \
-b:a 128k \
-threads 8 \
-pix_fmt yuv420p \
-f flv "rtmp://live.justin.tv/app/$STREAM_KEY"

