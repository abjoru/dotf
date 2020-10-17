#!/bin/bash
#
# nvim postinstall actions
#
# This will install spaceneovim with custom layers
CONFIG_DIR="$HOME/.config/nvim"
AUTOLOAD_DIR="$HOME/.config/nvim/autoload"
CACHE_DIR="$HOME/.cache/nvim"

AUTOLOAD_FILE="$HOME/.config/nvim/autoload/spaceneovim.vim"

mkdir -p "$CACHE_DIR" \
  && echo ">>> Creating autoload directory for spaceneovim" \
  && mkdir -p "$AUTOLOAD_DIR" \
  && echo ">>> Downloading spaceneovim core" \
  && curl -sSfL https://raw.githubusercontent.com/tehnix/spaceneovim/master/autoload/spaceneovim.vim -o "$AUTOLOAD_FILE" \
  && echo ">>> Launching nvim" \
  && nvim --cmd "let g:dotspaceneovim_do_not_run_bootstrap=1" +SpaceNeovimRunInstallProcess && nvim \
  && echo ">>> DONE!"

