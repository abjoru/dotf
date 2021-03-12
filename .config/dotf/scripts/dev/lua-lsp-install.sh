#!/bin/bash

. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

OS_SCRIPT=linux.ninja

if is_darwin; then
  OS_SCRIPT=macos.ninja
fi

pushd $DATA/lua-lsp
pushd 3rd/luamake
ninja -f ninja/$OS_SCRIPT
popd
./3rd/luamake/luamake rebuild
popd
