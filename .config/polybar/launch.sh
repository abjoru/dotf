#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar --reload m1 &
polybar --reload m2 &
polybar --reload m3 &
polybar --reload m4 &
