#!/bin/bash
#
# Pre install script for the desktop bundle
source ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Create XMonad data dir for XDG
mkdir -p $DATA/xmonad
