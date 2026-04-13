#!/bin/bash

if [ -f /etc/arch-release ]; then
    paru -S babashka-bin
else
    brew install borkdude/brew/babashka
fi
