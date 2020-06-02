#!/bin/sh

## WARNING: If started from TTY on a multi-seat, the user must run 'startx &
## vlock' to ensure the X locker cannot be bypassed.

## Run site init scripts. Usually not necessary.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

## Source environment config.
[ -f ~/.xprofile ] && . ~/.xprofile

if [ $# -eq 0 ]; then
    # remap caps lock to escape
    setxkbmap -option caps:escape
    # startup emacs
    # exec i3
    exec emacs
fi

exec "$@"
