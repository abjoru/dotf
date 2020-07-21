#!/bin/bash
TODAY=$(date +%Y-%m-%d)
CPDAY="2020-11-19"
DIFF=0

if [[ "$OSTYPE" == "darwin"* ]]; then
  d1=$(date -jf %Y-%m-%d $TODAY +%s)
  d2=$(date -jf %Y-%m-%d $CPDAY +%s)
  DIFF=$(( ($d2 - $d1) / (60*60*24) ))
else
  d1=$(date -d $TODAY +%s)
  d2=$(date -d $CPDAY +%s)
  DIFF=$(( ($d2 - $d1) / (60*60*24) ))
fi
  
if [ $DIFF -gt 0 ]; then
  echo " CyberPunk 2077: ${DIFF}D"
else
  echo " CyberPunk 2077 released!"
fi
