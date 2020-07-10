#!/bin/bash
#
# Stolen from: https://github.com/arcolinux/arcolinux-xmonad-xmobar/blob/master/etc/skel/.xmonad/scripts/autostart.sh

function run {
  if ! pgrep $1 ; then
    $@&
  fi
}

# cursor active at boot
xsetroot -cursor_name left_ptr &

# start conky for shortcut hints
#(conky -c $HOME/.config/xmonad/scripts/system-overview) &

# starting utility applications at boot 
picom --config $HOME/.config/xmonad/scripts/picom.conf &

# Polybar
$HOME/.config/polybar/launch.sh &

# starting user applications at boot
nitrogen --restore &

# sys-tray apps
synology-drive &
megasync &
volumeicon &
