#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# xset +fp /user/share/fonts/local/ &
xrdb -merge $HOME/dotfiles/.Xresources
xrdb -merge $HOME/dotfiles/Xresources/jellybeans

razercfg -r 400

# xsetroot -solid "#151a25"
# feh --bg-fill $HOME/bg.png &
# xsetroot -cursor_name left_ptr &

# xcompmgr -c -f -t3 -l3 -r2 -o.5 &

# conky &

# exec dwm
# exec frankenwm
exec startxfce4

# vim: set ft=sh:

