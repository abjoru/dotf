export ZSH=${XDG_CONFIG_HOME:-$HOME/.config}

setopt extended_glob

# Make sure we have XDG bin directory on our path (looking at you OSX)
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
  export PATH=$HOME/.local/bin:$PATH
fi

# Stash your environment variables in ~/.localrc. This means they'll stay out
# of your main dotfiles repository (which may be public, like this one), but
# you'll have access to them in your scripts.
if [[ -a $ZDOTDIR/.localrc ]]
then
  source $ZDOTDIR/.localrc
fi

# all of our zsh files
typeset -U config_files
config_files=(${XDG_CONFIG_HOME:-$HOME/.config}/**^external/*.zsh)

# load the path files
for file in ${(M)config_files:#*/path.zsh}
do
  source $file
done

# load everything but the path and completion files
for file in ${${config_files:#*/path.zsh}:#*/completion.zsh}
do
  source $file
done

# initialize autocomplete here, otherwise functions won't be loaded
autoload -Uz compinit
compinit

# load every completion after autocomplete loads
for file in ${(M)config_files:#*/completion.zsh}
do
  source $file
done

unset config_files

# Keybindings

# Bind ctrl + space to accept autosuggest
bindkey '^ ' autosuggest-accept

unsetopt ignoreeof

#export VISUAL=nvim
#autoload edit-command-line; zle -N edit-command-line
#bindkey -M vicmd v edit-command-line

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# TODO move to xdg dir
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# Fancy boot screen
neofetch
