#!/bin/bash
if [[ "$OSTYPE" == "linux-gnu"* && $(which apt) == 0 ]]; then
  if [ ! -e "/etc/apt/sources.list.d/mmstick76-ubuntu-alacritty-focal.list" ]; then
    sudo add-apt-repository ppa:mmstick76/alacritty
  fi
fi
