#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# trayer --edge top --align right --SetDockType true --SetPartialStrut true \
#  --expand true --width 10 --transparent true --tint 0x000000 --height 21 --alpha 0 &

unclutter &
pasystray &
nm-applet &
cbatticon &
blueman-applet &
xrandr -s 1920x1080
xsetroot -cursor_name left_ptr
# dim screen after 9:50, lock after 10 min
xset s 590 10
# turn screen off 10 seconds after lock
xset +dpms dpms 610 610 610
xss-lock -n /usr/lib/xsecurelock/dimmer -l -- dunst_lock &
picom &
nitrogen --restore
exec awesome
