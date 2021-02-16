#!/bin/bash
#
# Post install scripts for the dev bundle
. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Create XDG data dir for tig
if [ ! -d "$DATA/tig" ]; then
  mkdir -p $DATA/tig
fi
