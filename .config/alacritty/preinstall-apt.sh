#!/bin/bash
#
# preinstall-apt.sh
#

# Add PPA repo for Alacritty if it does not exist
if [ ! -f "/etc/apt/sources.list.d/mmstick76-ubuntu-alacritty-focal.list" ]; then
  sudo add-apt-repository ppa:mmstick76/alacritty
fi
