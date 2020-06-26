#!/bin/bash
#
# get-volume.sh
#
# Get the maximum volume of any pulseaudio sink channel
vol=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "MM" } else { print $2 }}' | head -n 1)
echo Vol: $vol%
exit 0
