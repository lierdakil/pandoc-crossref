#!/bin/bash

stack --no-terminal setup --resolver=ghc-$GHCVER
stack --no-terminal install cabal-install --resolver=nightly --compiler=ghc-$GHCVER

resf="ghc-$GHCVER.yaml"
echo "resolver: ghc-$GHCVER" > "$resf"
echo "packages: ['.']" >> "$resf"
stack --no-terminal solver --update-config --stack-yaml="$resf" || exit 101
sed -i 's/^resolver:/compiler:/;s/^extra-deps:/packages:/' "$resf"
sed -i '/^compiler:/ i - pretty-show-1.6.12\n- haskell-lexer-1.0.1\n- happy-1.19.5' "$resf"
echo "resolver: { name: 'ghc-$GHCVER', location: './$resf' }" > stack.yaml
echo "packages: ['.']" >> stack.yaml
ir=$( stack path --snapshot-install-root )
ls -d ${ir%/custom-ghc-*}/custom-ghc-* | grep -v "${ir%/*}" | while read i; do
  rm -rfv "$i"
done
