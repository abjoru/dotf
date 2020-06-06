if [ "$OSTYPE" == "linux-gnu"* ]; then
  #chsh -s $(which zsh)
  sudo usermod -s $(which zsh) $(whoami)
fi
