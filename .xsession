#!/bin/bash

function run {
  if ! pgrep $1 ; then
    $@&
  fi
}

XHOME=${XDG_CONFIG_HOME:-$HOME/.config}/xmonad

# Load resources
xrdb -merge .Xresources

# Set cursor
xsetroot -cursor_name left_ptr &

# Trayer
#trayer --edge top \
      #--align right \
      #--widthtype request \
      #--padding 6 \
      #--SetDockType true \
      #--SetPartialStrut true \
      #--expand true \
      #--monitor 1 \
      #--transparent true \
      #--alpha 0 \
      #--tint 0x292d3e \
      #--height 22 &
stalonetray -c $XHOME/stalonetrayrc &

# Start composite manager
picom --config $XHOME/scripts/picom.conf &

# Reload backgrounds
nitrogen --restore &

# External media
udiskie --tray &

# Launch apps
run volumeicon
run synology-drive
run megasync
run streamdeck

# Attempt to start telegram in tray
telegram-desktop -startintray &
