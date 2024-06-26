#!/bin/bash

branch="$(git branch --show-current)"

pd=pandoc
pc=pandoc-crossref

find test/m2m -iname 'input.md' -print | while read i; do
  echo $i
  dn="$(dirname "$i")"
  bdn="$(basename "$dn")"
  listings=""
  if [ "${bdn%%-*}" == "listings" ]; then
    listings="--listings"
  fi
  "$pd" -F "$pc" "$i" -t markdown-raw_attribute-raw_html -o "$dn/expect.md"
  "$pd" -F "$pc" "$i" $listings -t latex -o "$dn/expect.tex"
done
