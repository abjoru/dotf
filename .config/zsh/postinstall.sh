#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu"* && "$SHELL" != *"zsh"* ]]; then
  #chsh -s $(which zsh)
  sudo usermod -s $(which zsh) $(whoami)
fi
