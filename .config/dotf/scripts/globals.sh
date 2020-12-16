#!/bin/bash

###############
# Directories #
###############

DATA=${XDG_DATA_HOME:-$HOME/.local/share}
CACHE=${XDG_CACHE_HOME:-$HOME/.cache}
CONFIG=${XDG_CONFIG_HOME:-$HOME/.config}

#############
# Functions #
#############

is_arch() {
  if [[ "$OSTYPE" == "linux-gnu"* && -x "$(command -v pacman)" ]]; then
    return true
  else 
    return false
  fi
}

is_debian() {
  if [[ "$OSTYPE" == "linux-gnu"* && -x "$(command -v apt)" ]]; then
    true; return
  else 
    false; return
  fi
}

is_linux() {
  if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    true; return
  else 
    false; return
  fi
}

is_darwin() {
  if [[ "$OSTYPE" == "darwin"* ]]; then
    true; return
  else
    false; return
  fi
}
