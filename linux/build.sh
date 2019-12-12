#!/bin/bash
CMD=\
"git clone --bare /mnt ./.git
git config --unset core.bare
git reset --hard HEAD
cabal new-install pandoc $CABAL_OPTS
cabal new-build --jobs exe:pandoc-crossref $CABAL_OPTS
find dist-newstyle -type f -perm +100 -name pandoc-crossref -exec cp {} ./ \;
if [ -n \"$RUN_UPX\" ]; then
  upx --best pandoc-crossref
fi
export PANDOC=\"/root/.cabal/bin/pandoc\"
\$PANDOC -s -t man docs/index.md -o pandoc-crossref.1
PANDOCVER=\$(\$PANDOC --version | head -n1 | cut -f2 -d' ' | tr '.' '_')
tar czf \"/mnt/linux-pandoc_\$PANDOCVER.tar.gz\" ./pandoc-crossref ./pandoc-crossref.1
cp ./pandoc-crossref /mnt/
./inttest.sh
"

if [ -z "$DOCKER_IMAGE_VERSION" ]; then
  DOCKER_IMAGE_VERSION=staging
fi

docker pull lierdakil/pandoc-crossref-build:$DOCKER_IMAGE_VERSION
docker run --rm -v "$PWD:/mnt" lierdakil/pandoc-crossref-build:$DOCKER_IMAGE_VERSION /bin/ash -c "$CMD"
