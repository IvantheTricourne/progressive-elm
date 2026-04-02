#!/bin/bash
# build.sh — compile Elm and copy output next to index.html
set -e

echo "▶ Compiling Elm..."
elm make src/Main.elm --output=main.js --optimize

echo "✓ Done — open index.html in a browser"
