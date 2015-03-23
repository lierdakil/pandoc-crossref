#!/bin/bash

diff demo.native <(pandoc -F pandoc-crossref.hs -i demo.md -t native)
