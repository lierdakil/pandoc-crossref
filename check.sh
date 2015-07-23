#!/bin/bash

diff demo.native <(pandoc -F dist/build/pandoc-crossref/pandoc-crossref -i demo.md -t native)
diff demo-chapters.native <(pandoc -F dist/build/pandoc-crossref/pandoc-crossref -i demo.md -t native --chapters -M chapters)
