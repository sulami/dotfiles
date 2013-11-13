#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Xresources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
compton -cb -r 5 -l -5 -t -5 -o 1 --clear-shadow
xrandr --output DVI-1 --off
xrandr --output DVI-0 --mode 1920x1080
feh --bg-center ~/bg.png
exec monsterwm
