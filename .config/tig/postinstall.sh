#!/bin/bash
#
# tig postinstall.sh
#

if [ ! -d "${XDG_DATA_HOME:-$HOME/.local/share}/tig" ]; then
  mkdir ${XDG_DATA_HOME:-$HOME/.local/share}/tig
fi
