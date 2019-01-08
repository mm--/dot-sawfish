#!/bin/bash

# Hack until I get the path set correctly in Sawfish
. $HOME/.nix-profile/etc/profile.d/nix.sh

TEXT=$(maim -s | tesseract -l eng - -)
echo "$TEXT" | xclip -i
notify-send Copied: "$TEXT"
