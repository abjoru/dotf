#!/bin/bash
#
# Stolen from: https://github.com/arcolinux/arcolinux-xmonad-xmobar/blob/master/etc/skel/.xmonad/scripts/picom-toggle.sh

if pgrep -x "picom" > /dev/null ; then
  killall picom
else
  picom -b --config ~/.config/xmonad/scripts/picom.conf
fi
