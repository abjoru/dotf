#!/bin/bash
#
# Stolen from: https://github.com/arcolinux/arcolinux-xmonad-xmobar/blob/master/etc/skel/.xmonad/scripts/autostart.sh

# starting utility applications at boot 
picom --config $HOME/.config/xmonad/scripts/picom.conf &

# sys-tray apps
sleep 5
streamdeck &
