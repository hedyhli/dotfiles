#!/usr/bin/env sh

# This script should be run at start up by sh-like shells
# This of course does not include fish.
# For fish see ~/dotscripts/setup/fish and ~/.config/fish/config.fish

# Determine current running shell.
# https://unix.stackexchange.com/a/72475
if test -n "$ZSH_VERSION"; then
  CURSHELL=zsh
elif test -n "$FISH_VERSION"; then
  CURSHELL=fish
elif test -n "$BASH_VERSION"; then
  CURSHELL=bash
elif test -n "$KSH_VERSION"; then
  CURSHELL=ksh
elif test -n "$FCEDIT"; then
  CURSHELL=ksh
elif test -n "$NU_VERISON"; then
  CURSHELL=nu
elif test -n "$PS3"; then
  CURSHELL=unknown
else
  CURSHELL=sh
fi

# Aliases
source ~/.aliases

# Add the paths
export PATH="$(~/dotscripts/convert/addpath):$PATH"

# Hook (the) direnv
if ! command -v direnv &> /dev/null; then
    # TODO: Ignore if direnv hook retured status 1
    eval "$(direnv hook $CURSHELL)"
fi

# export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
# ^ Above is commented because it's already in ~/.exportenvs
# Load (the) nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$HOME/.exportenvs" ] && \. "$HOME/.exportenvs"

# Pyenv
if ! command -v pyenv &> /dev/null; then
    eval "$(pyenv init -)"
fi
