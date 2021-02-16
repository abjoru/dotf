#!/bin/bash
#
# Post install scripts for the dev bundle
. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Write completion script for DotF
# TODO base install might not have xdg bin dir in path!
# default to ~/.local/bin/dotf if which fails
if which dotf; then
  dotf --zsh-completion-script `which dotf` >> $CONFIG/zsh/functions/_dotf
else
  dotf --zsh-completion-script $HOME/.local/bin/dotf >> $CONFIG/zsh/functions/_dotf
fi

# Update shell to ZSH
if is_linux; then
  chsh -s /usr/bin/zsh
fi
