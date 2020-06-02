#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

[ -f ~/.xprofile ] && . ~/.xprofile

if [ $# -eq 0 ]; then
    setxkbmap -option caps:escape
    exec emacs
fi

exec "$@"
