#!/bin/bash

export PATH="/usr/local/opt/ghc@8.6/bin:$PATH"
echo "GHC version is:"
ghc --version
export PANDOC="$HOME/.cabal/bin/pandoc"
rm "$PANDOC" || true
cabal v2-update
cabal v2-install pandoc $CABAL_OPTS
cabal v2-install exe:pandoc-crossref --installdir=. --install-method=copy $CABAL_OPTS
if [ -n "$RUN_UPX" ]; then
  upx --best pandoc-crossref
fi
$PANDOC -s -t man docs/index.md -o pandoc-crossref.1
PANDOCVER=$($PANDOC --version | head -n1 | cut -f2 -d' ' | tr '.' '_')
echo "Pandoc exe version is:"
$PANDOC --version
tar czf "macos-pandoc_$PANDOCVER.tar.gz" ./pandoc-crossref ./pandoc-crossref.1
./inttest.sh
