#!/bin/bash

set -ue

cd ~/.password-store/
FILE=$(find . -type f -name "*.gpg" | sed 's|./||; s|.gpg$||' | peco)
pass -c "$FILE"

sleep 10



