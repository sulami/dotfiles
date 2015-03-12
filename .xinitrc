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

xset -dpms &
xset s off &
xset r rate 200 20 &

# razercfg -r 400 &

sxhkd -c ~/dotfiles/.sxhkdrc &

if [[ -e ~/.fehbg ]]; then
    sh ~/.fehbg
fi

# xsetroot -solid "#151a25"
# feh --bg-fill $HOME/bg.png &
# xsetroot -cursor_name left_ptr &

compton -c -C -f -t-5 -l-5 -r8 -o.5 &

# conky -c $HOME/dotfiles/.conkyrc &

# This sets the resolution properly after slim mangled it.
~/dotfiles/scripts/swapdisplays.sh &

# exec dwm
# exec frankenwm
# exec cwm
# exec startxfce4
exec 2bwm

# vim: set ft=sh:

