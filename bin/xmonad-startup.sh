#!/bin/dash
pkill compton
pkill dunst
pkill stalonetray
pkill urxvtd
pkill xscreensaver
pkill xss-lock

compton -CGb &
dunst &
stalonetray &
urxvtd -q -o -f &
#"trayer --edge top --align right --widthtype percent --height 24 --alpha 0 --transparent true --width 5 -tint 0x282c34"
xscreensaver -no-splash &
xss-lock -v -l ${HOME}/bin/screenlock.sh &
