#!/usr/bin/env bash

pd=pandoc
pc=pandoc-crossref

find test/m2m -iname 'input.md' -print | while read i; do
  echo $i
  dn="$(dirname "$i")"
  bdn="$(basename "$dn")"
  listings=""
  standalone=""
  if [ "${bdn%%-*}" == "listings" ]; then
    listings="--listings"
  fi
  if [ "${bdn%%-*}" == "standalone" ]; then
    standalone="--standalone"
  fi
  "$pd" -F "$pc" "$i" $standalone -t markdown-raw_attribute-raw_html -o "$dn/expect.md"
  "$pd" -F "$pc" "$i" $listings -t latex -o "$dn/expect.tex"
done
