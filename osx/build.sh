#!/bin/bash

export STACK_YAML=stack-release.yaml

unset CC
export PATH=/opt/ghc/$GHCVER/bin:$PATH
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
travis_retry curl -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 -C ~/.local/bin
stack --no-terminal setup
stack --no-terminal build

# deployment
stack --no-terminal install
stack --no-terminal exec -- pandoc -s -t man docs/index.md -o $HOME/.local/bin/pandoc-crossref.1
PANDOCVER=$(stack --no-terminal exec -- --version | head -n1 | cut -f2 -d' ' | tr '.' '_')
RELEASE_FN="macos-pandoc_$PANDOCVER.tar.gz"
tar czf "$TRAVIS_BUILD_DIR/$RELEASE_FN" -C "$HOME/.local/bin/" "./pandoc-crossref" "./pandoc-crossref.1"
