#!/bin/sh
#
# Init script for dotfiles.
#
# Based on the article 'The best way to store your dotfiles: A bare Git repository':
# https://www.atlassian.com/git/tutorials/dotfiles

# Grab GIT if we need to..
if [ ! -x "$(which git)" ]; then
  if [[ "$OSTYPE" == "darwin"* ]]; then
    brew install git
  elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(which apt)" ]]; then
    sudo apt install -y git
  elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(which pacman)" ]]; then
    sudo pacman --noconfirm -S git
  fi
fi

git clone --bare https://github.com/abjoru/dotf.git $HOME/.dotf

function config {
  $(which git) --git-dir=$HOME/.dotf/ --work-tree=$HOME $@
}

mkdir -p .config-backup
config checkout

if [ $? = 0 ]; then
  echo "Checked out config."
else
  echo "Backing up pre-existing dot files."
  config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
fi

config checkout
config config status.showUntrackedFiles no

# Ask for Java install..
if [ ! -x "$(which java)" ]; then
  echo "Missing Java runtime environment!"
  echo
  echo "Installing default JRE"

  if [[ "$OSTYPE" == "darwin"* ]]; then
    brew install openjdk
  elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(which apt)" ]]; then
    sudo apt install -y default-jdk
  elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(which pacman)" ]]; then
    sudo pacman --noconfirm -S jre-openjdk
  fi
fi

# Set path so that we can use 'dotf'
source $HOME/.local/share/dotf/temp-exports.sh
