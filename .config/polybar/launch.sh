#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

#for m in $(polybar --list-monitors | cut -d":" -f1); do
  #WIRELESS=$(ls /sys/class/net/ | grep ^wl | awk 'NR==1{print $1}') MONITOR=$m polybar --reload mainbar-xmonad &
#done

# Default to standard monitor unless multiple
monitors=( $(polybar --list-monitors) )
if [ ${#monitors[@]} -eq 1 ]; then
  MONITOR=${monitors[0]} polybar --reload m1 &
else
  polybar --reload m1 &
  polybar --reload m2 &
  polybar --reload m3 &
  polybar --reload m4 &
fi

echo "Bars launched..."
