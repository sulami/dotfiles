#!/bin/sh

# exec dwm
# exec frankenwm
# exec cwm
# exec startxfce4
exec ck-launch-session dbus-launch --sh-syntax --exit-with-session startxfce4 &
sh ~/.xprofile
pulseaudio --start
# exec 2bwm

# vim: set ft=sh:

