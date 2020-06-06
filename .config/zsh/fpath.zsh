# add each XDG config folder to fpath so that
# they can add functions and completion scripts
for fld (${XDG_CONFIG_HOME:-$HOME/.config}/*) if [ -d $fld ]; then
  FPATH=($fld $FPATH)
fi

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
fi
