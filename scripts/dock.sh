#!/bin/sh

# Do everything that needs to be done when docking/undocking my notebook.

# Figure out if we are docked.
get_dock_status()
{
    if [ $(cat /sys/devices/platform/dock.2/docked) = "1" ]; then
        return 0
    else
        return 1
    fi
}

# Switch the display.
switch_display()
{
    if get_dock_status; then
        xrandr --output DP2 --mode 1920x1080 --output LVDS1 --off
    else
        xrandr --output LVDS1 --mode 1280x800
    fi
}

# Change the terminal colourscheme.
switch_colours()
{
    if get_dock_status; then
        xrdb --merge $HOME/dotfiles/Xresources/su256
    else
        xrdb --merge $HOME/dotfiles/Xresources/solarized-light
    fi
}

# Do everything we want to do on dock/undock.
action()
{
    switch_display
    # switch_colours
}

action

