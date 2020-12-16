#!/bin/sh
#source https://github.com/x70b1/polybar-scripts

if [[ "$OSTYPE" == "linux-gnu"* && -x "$(command -v apt)" ]]; then
  if ! updates_apt=$(/usr/lib/update-notifier/apt-check -p | wc -l); then
    updates_apt=0
  fi

  if [ "$updates_apt" -gt 0 ]; then
    echo "<fc=$1> $updates_apt</fc>"
  else
    echo "<fc=$1> 0</fc>"
  fi
elif [[ "$OSTYPE" == "linux-gnu"* && -x "$(command -v pacman)" ]]; then
  if ! updates_arch=$(checkupdates 2> /dev/null | wc -l); then
    updates_arch=0
  fi

  if ! updates_aur=$(trizen -Su --aur --quiet | wc -l); then
    updates_aur=0
  fi

  updates=$(("$updates_arch" + "$updates_aur"))

  if [ "$updates" -gt 0 ]; then
    echo "<fc=$1> $updates</fc>"
  else
    echo "<fc=$1> 0</fc>"
  fi
fi
