{-# LANGUAGE CPP #-}
module Native where

import Text.Pandoc.Definition

demo, demochapters :: [Block]

demo =
#include "demo.inc"
demochapters =
#include "demo-chapters.inc"
