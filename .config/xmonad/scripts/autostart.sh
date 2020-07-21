#!/bin/bash
#
# Stolen from: https://github.com/arcolinux/arcolinux-xmonad-xmobar/blob/master/etc/skel/.xmonad/scripts/autostart.sh

function run {
  if ! pgrep $1 ; then
    $@&
  fi
}

# start conky for shortcut hints
(conky -c $HOME/.config/xmonad/scripts/system-overview) &

# starting utility applications at boot 
run picom --config $HOME/.config/xmonad/scripts/picom.conf
