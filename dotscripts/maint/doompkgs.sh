#!/usr/bin/env bash


cd ~/.config/doom/
wget https://raw.githubusercontent.com/doomemacs/doomemacs/master/templates/init.example.el
nv init.el "+vsp init.example.el"
