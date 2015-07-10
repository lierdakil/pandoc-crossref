#!/bin/bash

diff demo.native <(pandoc -F dist/build/pandoc-crossref/pandoc-crossref -i demo.md -t native)
