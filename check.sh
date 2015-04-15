#!/bin/bash

diff demo.native <(pandoc -F pandoc-crossref -i demo.md -t native)
