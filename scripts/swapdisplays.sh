#!/usr/bin/zsh

RESOLUTION="1920"
STATUS=$(xrandr | grep "DVI-1")

if test "${STATUS#*$RESOLUTION}" != $STATUS
then
    xrandr --output DVI-1 --off --output DVI-0 --mode 1680x1050
else
    xrandr --output DVI-0 --off --output DVI-1 --mode 1920x1080
fi

