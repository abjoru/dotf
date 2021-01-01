#!/bin/bash
. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

if is_debian; then
    # Add PPA repo for Alacritty if it does not exist
  if [ ! -f "/etc/apt/sources.list.d/mmstick76-ubuntu-alacritty-focal.list" ]; then
    sudo add-apt-repository ppa:mmstick76/alacritty
  fi
fi
