# Install yarn repo for debian based systems
if [[ "$OSTYPE" == "linux-gnu"* && $(which apt) == 0 ]]; then
  if [ ! -e "/etc/apt/sources.list.d/yarn.list" ]; then
    curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
  fi
fi
