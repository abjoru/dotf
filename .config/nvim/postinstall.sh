#!/bin/bash
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [ ! -e "${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/autoload/plug.vim" ]; then
    curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

    # Install all plugins
    nvim --headless +PlugInstall +qall
  fi
fi
