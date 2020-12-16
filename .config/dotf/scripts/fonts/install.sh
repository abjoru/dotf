#!/bin/bash
source ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

function getFont() {
  echo "Downloading $1"
  wget "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/$1.zip"
  unzip $1.zip -d $DATA/fonts
}

if is_debian; then
  dir=CACHE/nerdfonts

  if [ ! -d "$dir" ]; then
    mkdir -p $dir
    pushd $dir
    getFont UbuntuMono
    getFont Terminus
    getFont RobotoMono
    getFont Mononoki
    fc-cache -fv
    popd
  fi
fi

