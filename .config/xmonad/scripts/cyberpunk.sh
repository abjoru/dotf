#!/bin/bash
TODAY=$(date +%Y-%m-%d)
CPDAY="2020-12-10"

if [[ "$OSTYPE" == "darwin"* ]]; then
  d1=$(date -jf %Y-%m-%d $TODAY +%s)
  d2=$(date -jf %Y-%m-%d $CPDAY +%s)
else
  d1=$(date -d $TODAY +%s)
  d2=$(date -d $CPDAY +%s)
fi

DIFF=$(( ($d2 - $d1) / (60*60*24) ))

if [ $DIFF -gt 0 ]; then
  echo "<fc=$1>  CyberPunk 2077: ${DIFF}D</fc>"
else
  echo "<fc=$1>  CyberPunk 2077 released!</fc>"
fi

