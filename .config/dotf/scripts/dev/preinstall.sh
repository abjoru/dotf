#!/bin/bash
#
# Pre install script for the dev bundle
Source ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Debian only
if is_debian; then

  # Fetch yarn repo
  if [ ! -f /etc/apt/sources.list.d/yarn.list ]; then
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
    sudo apt update
  fi
fi