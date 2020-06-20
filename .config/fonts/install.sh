#!/bin/bash

function getFont() {
  echo "Downloading $1"
  wget "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/$1.zip"
  unzip $1.zip -d ${XDG_DATA_HOME:-$HOME/.local/share}/fonts
}

if [[ "$OSTYPE" == "darwin"* ]]; then
  brew tap homebrew/cask-fonts
elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(which apt)" ]]; then
  dir=${XDG_CACHE_HOME:-$HOME/.cache}/nerdfonts

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
