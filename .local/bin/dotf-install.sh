#!/bin/sh
#
# Init script for dotfiles.
#
# Based on the article 'The best way to store your dotfiles: A bare Git repository':
# https://www.atlassian.com/git/tutorials/dotfiles

# Grab GIT if we need to..



if [ ! -x "$(command -v git)" ]; then
  if [[ "$OSTYPE" == "darwin"* ]]; then
    brew install git
  elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(command -v apt)" ]]; then
    sudo apt install -y git
  elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(command -v pacman)" ]]; then
    sudo pacman --noconfirm -S git
  fi
fi

function config {
  $(which git) --git-dir=$HOME/.dotf/ --work-tree=$HOME $@
}

# Ask for git global username if not set
if [ -z "$(git config --global --get user.name)" ]; then
  echo
  read -p "[GIT] What is your full name: " username
  git config --global user.name "$username"
fi

# Ask for git global email if not set
if [ -z "$(git config --global --get user.email)" ]; then
  echo
  read -p "[GIT] What is your email: " useremail
  git config --global user.email "$useremail"
fi

if [[ ! -d "$HOME/.dotf" ]]; then
  git clone --bare https://github.com/abjoru/dotf.git $HOME/.dotf

  mkdir -p .config-backup
  config checkout

  if [ $? = 0 ]; then
    echo "Checked out config."
  else
    echo "Backing up pre-existing dot files."
    # TODO this won't work if the target path does not exist. i.e. existing xdg dirs
    config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
  fi

  config checkout
  config config status.showUntrackedFiles no
fi

# Ask for Java install..
if [ ! -x "$(command -v java)" ]; then
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
#source $HOME/.local/share/dotf/temp-exports.sh
if [[ "$SHELL" == *"bash"* ]]; then
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> $HOME/.bashrc
  echo
  echo "Restart your shell to gain access to the 'dotf' command!"
  echo "- Run 'dotf -d upgrade' to see what will be installed prior to an actual upgrade."
  echo "- Run 'dotf -h' for help."
  echo
fi
