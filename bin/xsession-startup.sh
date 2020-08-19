#!/bin/dash

pkill picom
pkill urxvtd
pkill steam
pkill syncthing-gtk
pkill nm-applet
pkill redshift-gtk
pkill pasystray

if command -v picom &> /dev/null; then
   picom -CGb
fi

if command -v urxvtd &> /dev/null; then
    urxvtd -q -o -f
fi

if command -v xsetroot  &> /dev/null; then
    xsetroot -cursor_name left_ptr
fi

if command -v xset &> /dev/null; then
    xset s off
fi

if command -v ibus-daemon &> /dev/null; then
    ibus-daemon -rdx
fi

if command -v diodon &> /dev/null; then
    diodon &
fi

if command -v syncthing-gtk &> /dev/null; then
    syncthing-gtk &
fi

if command -v nm-applet &> /dev/null; then
    nm-applet &
fi

if command -v radeon-profile &> /dev/null; then
    radeon-profile &
fi

if command -v flameshot &> /dev/null; then
    flameshot &
fi

if command -v redshift-gtk  &> /dev/null; then
    redshift-gtk &
fi

if command -v pasystray &> /dev/null; then
    pasystray &
fi
