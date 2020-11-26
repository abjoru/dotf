#!/bin/bash

# TODO Make sure pkg scripts are executed first! so that we get stack!
DATA_DIR=${XDG_DATA_HOME:-$HOME/.local/share}

# This pkg does not apply for OSX!
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  # Check for haskell stack
  if [ ! -x "$(command -v stack)" ]; then
    echo "ERROR: Haskell Stack was not found!"
    echo
    echo "This package should have been installed by the pkg script."
    echo "Please ensure that Stack is installed and try again..."
    exit 1
  fi

  # Checkout blueberry
  git clone https://github.com/abjoru/blueberry-mobar.git $DATA_DIR/blueberry-mobar

  # Call build.sh in blueberry
  pushd $DATA_DIR/blueberry-mobar
  sh build.sh
  popd
fi
