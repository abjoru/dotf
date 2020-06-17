#!/bin/bash
#
# nvim postinstall actions

# Make sure we have vim-plug installed so that we can bootstrap nvim
if [ ! -f "${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/autoload/plug.vim" ]; then
  curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim 
  nvim --headless +PlugInstall +qall
fi
