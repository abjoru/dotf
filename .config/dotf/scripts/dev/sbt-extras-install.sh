#!/bin/bash

. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

SBT_LAUNCHER=$BIN/sbt
SRC_PATH=$DATA/sbt-extras/sbt

# Check if sbt already exists
if [ -f "$SBT_LAUNCHER" ]; then
  # Check if we have a symlink
  if [ -L "$SBT_LAUNCHER" ]; then
    # Check if it is symlinked to git repo
    if [ "$SBT_LAUNCHER" -ef "$SRC_PATH" ]; then
      echo "SBT is already linked..."
    else
      # Unlink and re-link
      echo "Re-linking $SRC_PATH to $SBT_LAUNCHER..."
      unlink $SBT_LAUNCHER
      ln -s $SRC_PATH $SBT_LAUNCHER
    fi
  else
    # Remove non-symlinked and link to git repo
    echo "Removing old binary and linking $SRC_PATH to $SBT_LAUNCHER..."
    rm $SBT_LAUNCHER
    ln -s $SRC_PATH $SBT_LAUNCHER
  fi
else
  # Create symlink
  echo "Creating link from $SRC_PATH to $SBT_LAUNCHER..."
  ln -s $SRC_PATH $SBT_LAUNCHER
fi
