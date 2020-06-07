#!/bin/sh
# .config/fonts/preinstall.sh
#
# Installs 'all' NerdFonts from git repo
dir=${XDG_CACHE_HOME:-$HOME/.cache}/nerdfonts

if [ ! -d "$dir" ]; then
  mkdir -p $dir
  pushd $dir
  git clone --depth 1 https://github.com/ryanoasis/nerd-fonts.git
  sh -c nerd-fonts/install.sh
  popd
fi
