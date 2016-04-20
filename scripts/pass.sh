#!/bin/bash

set -ue

cd ~/.password-store/
FILE=$(find . -type f -name "*.gpg" | sed 's|./||; s|.gpg$||' | peco)
pass show "$FILE"
pass -c "$FILE"

sleep 45



