#!/bin/bash

dynamic-colors cycle

DARK="solarized-dark"
LIGHT="solarized-light"

CUR_THEME=$(<${HOME}/.dynamic-colors/colorscheme)

I3HOME=${HOME}/.config/i3
I3LOCALCFG=${I3HOME}/config.local
I3EXECFG=${I3HOME}/config.exec
I3CFG=${I3HOME}/config
GTKRC=${HOME}/.gtkrc-2.0
GTK3HOME=${HOME}/.config/gtk-3.0
GTK3INI=${GTK3HOME}/settings.ini
XRES=${HOME}/.Xresources
XRESLOCAL=${HOME}/.Xresources-local
XRESLIGHT=${HOME}/.Xresources-light
XRESDARK=${HOME}/.Xresources-dark

if [ "${CUR_THEME}" == "${DARK}" ]; then
	cat "${I3LOCALCFG}" "${I3HOME}/config.dark" "${I3EXECFG}" > "${I3CFG}"
	cat "${XRESLOCAL}" "${XRESDARK}" > "${XRES}"
	cp "${HOME}/.gtkrc-2.0.dark" "${GTKRC}"
	cp "${GTK3HOME}/settings.ini.dark" "${GTK3INI}"
	emacsclient -q -e "(load-theme 'sanityinc-solarized-dark t)"
elif [ "${CUR_THEME}" == "${LIGHT}" ]; then
	cat "${I3LOCALCFG}" "${I3HOME}/config.light" "${I3EXECFG}" > "${I3CFG}"
	cat "${XRESLOCAL}" "${XRESLIGHT}" > "${XRES}"
	cp "${HOME}/.gtkrc-2.0.light" "${GTKRC}"
	cp "${GTK3HOME}/settings.ini.light" "${GTK3INI}"
	emacsclient -q -e "(load-theme 'sanityinc-solarized-light t)"
else
	printf "no theme"
	exit 1
fi

gtk2-reload-theme.pl

xrdb -load ${XRES}

i3-msg restart
