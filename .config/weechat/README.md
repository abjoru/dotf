# WeeChat Install for Gitter Support

Until any of this becomes automated, please follow these instructions to get Gitter support (and vimode) installed.

## Grab Gitter Token

1. Connect to irc.gitter.im with your IRC client using SSL
2. Grab the `Server Password` token (40-character token)

## Configure WeeChat

1. Start weechat
2. For vimode: `/script install vimode.py`
3. Set SSL cert path if needed: `/set weechat.network.gnutls_ca_file "/etc/ssl/certs/ca-certificates.crt"`
  - OSX: `/usr/local/etc/openssl/cert.pem`
4. Add Gitter: `/server add gitter irc.gitter.im/6667 -ssl -autoconnect -password=<40-char token>`
5. Connect to Gitter: `/connect gitter`
