#!/usr/bin/env sh

# This script should be run at start up by sh-like shells
# This of course does not include fish.
# For fish see ~/dotscripts/setup/fish and ~/.config/fish/config.fish

# Determine current running shell.
# unix.stackexchange.com/questions/71121/ (comment by frostschutz)
CURSHELL="$(sed -re 's/\x0.*//' /proc/$$/cmdline)"

# Add the paths
export PATH="$(~/bin/parse_addpath | sed 's/ /:/g'):$PATH"

# Hook (the) direnv
if ! command -v direnv &> /dev/null; then
    # TODO: Ignore if direnv hook retured status 1
    eval "$(direnv hook $CURSHELL)"
fi
