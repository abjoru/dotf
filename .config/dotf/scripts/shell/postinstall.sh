#!/bin/bash
#
# Post install scripts for the dev bundle
. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

# Write completion script for DotF
dotf --zsh-completion-script `which dotf` >> $CONFIG/zsh/functions/_dotf

# Update shell to ZSH
if is_linux; then
  chsh -s /usr/bin/zsh
fi
