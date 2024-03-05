#!/usr/bin/env bash

pandoc -F pandoc-crossref -i docs/demo/demo.md -t native | sed 's/^/  /' > test/demo.inc
pandoc --top-level-division=chapter -Mchapters -F pandoc-crossref -i docs/demo/demo.md -t native | sed 's/^/  /' > test/demo-chapters.inc
