#!/bin/bash
#
# Pre install script for the desktop bundle
. ${XDG_CONFIG_HOME:-$HOME/.config}/dotf/scripts/globals.sh

set -e

# These are the possible bundles from PIA
# https://www.privateinternetaccess.com/openvpn/openvpn.zip
# https://www.privateinternetaccess.com/openvpn/openvpn-strong.zip
# https://www.privateinternetaccess.com/openvpn/openvpn-tcp.zip
# https://www.privateinternetaccess.com/openvpn/openvpn-strong-tcp.zip

BASE_DIR=$CACHE/openvpn
BASE_URL="https://www.privateinternetaccess.com/openvpn"
BASE_CONFIG=us_florida.ovpn
CONFIG_BUNDLE=${CONFIG_BUNDLE:-"openvpn"}

# Check if bundle already downloaded!
if [ ! -f "${BASE_DIR}/dotf.ovpn" ]; then
  # Check if target dir exist
  if [ ! -d "$BASE_DIR" ]; then
    mkdir -p $BASE_DIR
  fi

  # Delete all files for PIA provider, except scripts
  find "$BASE_DIR" -type f ! -name "*.sh" -delete

  # Download and extract wanted bundle into temporary file
  tmp_file=$BASE_DIR/bundle.zip
  echo "Downloading OpenVPN config bundle $CONFIG_BUNDLE into temporary file $tmp_file"
  curl -sSL "${BASE_URL}/${CONFIG_BUNDLE}.zip" -o "$tmp_file"

  echo "Extracting OpenVPN config bundle into PIA directory $BASE_DIR"
  unzip -qjo "$tmp_file" -d "$BASE_DIR"
  rm "$tmp_file"

  # Remove default.ovpn if it exists (we'll create a new one)
  if [ -f "$BASE_DIR/default.ovpn" ]; then
    rm "$BASE_DIR/default.ovpn"
  fi

  # Copy and modify target config
  echo "Creating dotf.ovpn config file from $BASE_CONFIG"
  sed 's/auth-user-pass/auth-user-pass \/config\/openvpn-credentials.txt/' $BASE_DIR/$BASE_CONFIG >> $BASE_DIR/default.ovpn
else
  echo "OpenVPN config exists, skipping download..."
  exit 0
fi
