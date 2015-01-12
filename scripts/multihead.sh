#!/bin/sh

# Opens two Xephyr windows and connects them via Xinerama

Xephyr +extension RANDR :1 &
Xephyr +extension RANDR :2 &
Xdmx +xinerama -xinput local -display :1 -display :2 -ac :3

