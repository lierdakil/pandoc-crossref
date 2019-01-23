#!/bin/bash
CMD=\
"git clone --bare /mnt ./.git
git config --unset core.bare
git reset --hard HEAD
cabal new-build --jobs exe:pandoc-crossref $CABAL_OPTS
find dist-newstyle -type f -perm +100 -name pandoc-crossref -exec cp {} ./ \;
upx --ultra-brute --best pandoc-crossref
export PANDOC=\"\$(find /root/.cabal -type f -perm +100 -name pandoc -print)\"
\$PANDOC -s -t man docs/index.md -o pandoc-crossref.1
PANDOCVER=\$(\$PANDOC --version | head -n1 | cut -f2 -d' ' | tr '.' '_')
tar czf \"/mnt/linux-pandoc_\$PANDOCVER.tar.gz\" ./pandoc-crossref ./pandoc-crossref.1
"

docker pull lierdakil/pandoc-crossref-build:latest
docker run --rm -v "$PWD:/mnt" lierdakil/pandoc-crossref-build:latest /bin/ash -c "$CMD"
