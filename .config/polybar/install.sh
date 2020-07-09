#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  dir=${XDG_CONFIG_HOME:-$HOME/.config}/polybar/scripts
  if [[ ! -d "$dir/polybar-gmail-master" ]]; then
    pushd $dir
    curl -LO https://github.com/vyachkonovalov/polybar-gmail/archive/master.tar.gz
    tar zxf master.tar.gz && rm master.tar.gz
    popd

    # Obtain/refresh credentials
    ${XDG_CONFIG_HOME:-$HOME/.config}/polybar/scripts/polybar-gmail-master/auth.py
  fi
fi
