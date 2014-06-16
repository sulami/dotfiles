#!/bin/sh

ff="/tmp/monsterwm.fifo"
[[ -p $ff ]] || mkfifo -m 600 "$ff"

# desktop names
ds=("F1" "F2" "F3" "F4")

# layout names
ms=("[TILED]" "[MONOCLE]" "[BOTTOM STACK]" "[GRID]" "[FLOATING]")

while read -t 60 -r wmout || true; do
    if [[ $wmout =~ ^(([[:digit:]]+:)+[[:digit:]]+ ?)+$ ]]; then
        read -ra desktops <<< "$wmout" && unset r
        for desktop in "${desktops[@]}"; do
            IFS=':' read -r d w m c u <<< "$desktop"
            ((c)) && fg="%{F#FF597BC5}%{U#FF597BC5}" i="${ms[$m]}" \
            || fg="%{F#FFADADAD}%{U-}"
            ((u)) && w+='%{F#FFADADAD}%{U-}'
            # r+=" $fg${ds[$d]} [${w/#0/-}] "
            r+=" $fg${ds[$d]} "
        done
        r="${r%::*}"
    fi
    printf "%s%s%s\n" "%{l} $r" "%{c}%{F#FFADADAD}$i" "%{r}$(date +"%F  %R")  "
done < "$ff" | bar -d -g x18xx -u 2 -B "#FF121212" -F "#FF579BC5" \
-f "-*-terminus-*-*-*-12-*-*-*-*-*-*-*" &

# pass output to fifo
monsterwm > "$ff"

