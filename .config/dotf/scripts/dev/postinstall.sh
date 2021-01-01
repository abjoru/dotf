#!/bin/bash
#
# Post install scripts for the dev bundle
. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Create XDG data dir for tig
if [ ! -d "$DATA/tig" ]; then
  mkdir -p $DATA/tig
fi

# Grab VimPlug
curl -fLo $CONFIG/nvim/autoload/plug.vim --create-dirs \
  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Setup nvim dotf plugins
if [ ! -f "$CACHE/nvim/bootstrap.lock" ]; then
  mkdir -p $CACHE/nvim \
    && echo ">>> Launching nvim for plugin bootstrap" \
    && nvim +PlugUpdate \ #+DfInstall \
    && echo ">>> Done!"
else
  echo ">>> DotF neovim already installed!"
fi
