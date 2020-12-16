#!/bin/bash
source ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

if is_darwin; then
  # tap fonts
  brew tap homebrew/cask-fonts
fi
