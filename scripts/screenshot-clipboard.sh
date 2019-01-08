#!/bin/bash

# Hack until I get the path set correctly in Sawfish
. $HOME/.nix-profile/etc/profile.d/nix.sh

maim -s | xclip -selection clipboard -t image/png
