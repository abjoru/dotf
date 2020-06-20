#!/bin/bash
#
# linux-setup.sh

###################
# Shell Selection #
###################

if [[ "$SHELL" != *"zsh"* ]]; then
  read -p "Do you want to switch shell to ZSH (y/N)? " ans
  case ${ans:0:1} in
    y|Y)
      chsh -s $(which zsh)
      ;;
    *)
      echo "keeping $SHELL"
      ;;
  esac
fi
