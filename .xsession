#!/bin/bash

function run {
  if ! pgrep $1 ; then
    $@&
  fi
}

# Load resources
xrdb -merge .Xresources

# Set cursor
xsetroot -cursor_name left_ptr &

# Trayer
trayer --edge top \
      --align right \
      --widthtype request \
      --padding 6 \
      --SetDockType true \
      --SetPartialStrut true \
      --expand true \
      --monitor 1 \
      --transparent true \
      --alpha 0 \
      --tint 0x292d3e \
      --height 22 &

# Reload backgrounds
nitrogen --restore &

# Launch apps
run volumeicon
run synology-drive
run megasync
#run streamdeck # moved to xmonad autostart
run telegram-desktop

# Exec WM
exec xmonad
