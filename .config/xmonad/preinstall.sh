#!/bin/bash
#
# preinstall.sh

# Create XMonad data dir for XDG
mkdir -p ${XDG_DATA_DIR:-$HOME/.local/share}/xmonad
