#!/bin/bash
#
# aur.sh
pushd ${XDG_CACHE_DIR:-$HOME/.cache}
git clone https://aur.archlinux.org/yay-git.git
pushd yay
makepkg -si
popd
popd
