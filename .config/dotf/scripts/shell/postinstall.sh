#!/bin/bash
#
# Post install scripts for the dev bundle
. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Write completion script for DotF
# TODO base install might not have xdg bin dir in path!
# default to ~/.local/bin/dotf if which fails
if [ -f "$CONFIG/zsh/functions/_dotf" ]; then
  rm "$CONFIG/zsh/functions/_dotf"
fi

if which dotf; then
  dotf --zsh-completion-script `which dotf` >> $CONFIG/zsh/functions/_dotf
elif [ -f "$HOME/.local/bin/dotf" ]; then
  dotf --zsh-completion-script $HOME/.local/bin/dotf >> $CONFIG/zsh/functions/_dotf
else
  echo "DotF not found! Cannot create completions..."
fi

# Update shell to ZSH
# TODO Check shell and only change if not zsh!
if is_linux; then
  chsh -s /usr/bin/zsh
fi
