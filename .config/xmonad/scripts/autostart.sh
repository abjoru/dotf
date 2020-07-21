#!/bin/bash
#
# Stolen from: https://github.com/arcolinux/arcolinux-xmonad-xmobar/blob/master/etc/skel/.xmonad/scripts/autostart.sh

function run {
  if ! pgrep $1 ; then
    $@&
  fi
}

# cursor active at boot
#xsetroot -cursor_name left_ptr &

# starting user applications at boot
#nitrogen --restore &

# start conky for shortcut hints
#(conky -c $HOME/.config/xmonad/scripts/system-overview) &

# starting utility applications at boot 
run picom --config $HOME/.config/xmonad/scripts/picom.conf

# Polybar
#source $HOME/.config/polybar/launch.sh

# Trayer
#run trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 22

# sys-tray apps
sleep 1
run streamdeck
#run megasync
#run volumeicon
#run synology-drive
