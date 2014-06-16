#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# xset +fp /user/share/fonts/local/ &
xrdb ~/.Xresources &

# xsetroot -solid "#121212"
feh --bg-fill $HOME/bg.png &
xsetroot -cursor_name left_ptr &

xcompmgr -c -t3 -l3 -r2 -o.5 &

# exec dwm
exec $HOME/dotfiles/monsterwm/monsterwm2bar.sh

# vim: set ft=sh:

