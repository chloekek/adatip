#!/usr/bin/env bash

# This script is used by Hivemind.

set -efuo pipefail

cd adatipd/static

# Automatically rebuild Sass code when changed.
# -n: Make entr not ask for input on an interactive TTY.
find -type f -name '*.scss' |   \
    exec entr -nr -- sassc      \
        --precision 10          \
        stylesheet.scss         \
        stylesheet.css
