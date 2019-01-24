#!/bin/bash

DIFF="git --no-pager diff --"

: ${PANDOC:=pandoc}

t=$(tempfile) || exit
trap "rm -f -- '$t'" EXIT

for i in test/m2m/*; do
  $PANDOC -F ./pandoc-crossref "$i/input.md" -t markdown > "$t"
  $DIFF "$i/expect.md" "$t" || echo "Failed md: $i"
  $PANDOC -F ./pandoc-crossref "$i/input.md" -t latex > "$t"
  $DIFF "$i/expect.tex" "$t" || echo "Failed tex: $i"
done

rm -f -- "$t"
trap - EXIT
exit
