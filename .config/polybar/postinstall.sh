#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  dir=${XDG_CONFIG_HOME:-$HOME/.config}/polybar/scripts
  if [[ ! -d "$dir/polybar-gmail-master" ]]; then
    pushd $dir
    # TODO move this out to separate module/space ?
    pip install --upgrade google-api-python-client google-auth-httplib2 google-auth-oauthlib \
      yahoo_fin argparse pandas requests_html
    curl -LO https://github.com/vyachkonovalov/polybar-gmail/archive/master.tar.gz
    tar zxf master.tar.gz && rm master.tar.gz
    popd

    # Obtain/refresh credentials
    ${XDG_CONFIG_HOME:-$HOME/.config}/polybar/scripts/polybar-gmail-master/auth.py
  fi

  rules=/etc/udev/rules.d/95-usb.rules
  if [[ ! -f "$rules" ]]; then
sudo bash <<EOF
cat > $rules <<EOR
KERNEL=="sd*", ACTION=="add", ATTR{removable}=="1", \
    RUN+="/home/user/.config/polybar/system-usb-udev.sh --update"
KERNEL=="sd*", ACTION=="remove", \
    RUN+="/home/user/.config/polybar/system-usb-udev.sh --update"
EOR
EOF
  fi
fi
