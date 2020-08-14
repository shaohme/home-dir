#!/bin/dash
# set -e

pkill picom
# pkill dunst
# pkill stalonetray
pkill urxvtd
pkill steam
# pkill clipit
pkill syncthing-gtk
# pkill xscreensaver
pkill nm-applet
pkill redshift-gtk
pkill pasystray
/usr/bin/emacsclient --eval "(kill-emacs)"

picom -CGb
urxvtd -q -o -f
xsetroot -cursor_name left_ptr
xset s off
# xmodmap ~/.Xmodmap
ibus-daemon -rdx
# dunst &
# clipit &
diodon &
# stalonetray &

# run daemon from within X session as systemd seems
# to leave evironment variables for one thing
/usr/bin/emacs --daemon &
syncthing-gtk &
nm-applet &
# radeon-profile &
flameshot &
# xscreensaver &
redshift-gtk &
pasystray &
# steam -silent &
