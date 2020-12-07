#!/bin/bash
#
# Post install scripts for the dev bundle
source ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Create XDG data dir for tig
if [ ! -d "$DATA/tig" ]; then
  mkdir -p $DATA/tig
fi

# Setup nvim dotf plugins
if [ ! -f "$CACHE/nvim/bootstrap.lock" ]; then
  mkdir -p $CACHE/nvim \
    && echo ">>> Launching nvim for plugin bootstrap" \
    && nvim +DfInstall \
    && echo ">>> Done!"
else
  echo ">>> DotF neovim already installed!"
fi
