#!/bin/sh

xrdb -merge ~/.Xresources

xset s off

# xmonad needs this to show cursor at start
xsetroot -cursor_name left_ptr

# exec i3
exec xmonad
