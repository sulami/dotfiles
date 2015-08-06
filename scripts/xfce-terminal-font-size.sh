#!/bin/bash

REGEXPR='s/FontName.*/FontName=Monospace '$1'/g'
sed -i "$REGEXPR" ~/.config/xfce4/terminal/terminalrc

