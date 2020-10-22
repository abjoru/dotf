#!/bin/bash
#
# dotf nvim postinstall actions
CACHE_DIR="$HOME/.cache/nvim"
LOCK_FILE="$HOME/.config/nvim/bootstrap.lock"

# check for bootstrap lock file
if [ ! -f "$LOCK_FILE" ]; then
  mkdir -p "$CACHE_DIR" \
    && echo ">>> Launching nvim for plugin bootstrap" \
    && nvim +DfInstall \
    && echo ">>> Done!"
else
  echo ">>> DotF neovim already bootstrapped!"
fi
