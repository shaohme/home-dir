#!/bin/sh

# xrdb -load ~/.Xresources
xset b off
xset s off
xset -dpms
# pulseaudio --start --log-target=stderr

# exec /usr/bin/i3

xrdb -load ${HOME}/.Xresources
xmodmap ${HOME}/.Xmodmap

xcape -e "Hyper_L=Tab;Hyper_R=backslash" &

xss-lock -v -l ${HOME}/bin/transfer-sleep-lock-i3lock.sh &

/usr/bin/xsettingsd &

# [ -f /etc/xprofile ] && . /etc/xprofile
# [ -f ~/.xprofile ] && . ~/.xprofile
exec i3
# exec xmonad
