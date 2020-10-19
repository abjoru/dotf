#!/bin/bash
#
# dotf nvim postinstall actions
CACHE_DIR="$HOME/.cache/nvim"
LOCK_FILE="$HOME/.config/nvim/bootstrap.lock"

# check for bootstrap lock file
if [ ! -f "$LOCK_FILE" ]; then
  mkdir -p "$CACHE_DIR" \
    && echo ">>> Launching nvim for plugin bootstrap" \
    && nvim --cmd "let g:dotf_do_not_run_bootstrap=1" +DfInstall \
    && echo ">>> Done!"
else
  echo ">>> DotF neovim already bootstrapped!"
fi
