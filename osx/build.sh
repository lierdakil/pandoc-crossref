#!/bin/bash

cabal new-update
cabal new-build exe:pandoc-crossref $CABAL_OPTS
find dist-newstyle -type f -perm +100 -name pandoc-crossref -exec cp {} ./ \;
upx --ultra-brute --best pandoc-crossref
PANDOC="$(find "$HOME/.cabal" -type f -perm +100 -name pandoc -print)"
$PANDOC -s -t man docs/index.md -o pandoc-crossref.1
PANDOCVER=$($PANDOC --version | head -n1 | cut -f2 -d' ' | tr '.' '_')
tar czf "macos-pandoc_$PANDOCVER.tar.gz" ./pandoc-crossref ./pandoc-crossref.1
