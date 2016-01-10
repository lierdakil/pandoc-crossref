{-# LANGUAGE CPP #-}
module Native where

import Text.Pandoc.Definition

demo, demochapters :: [Block]

#if MIN_VERSION_pandoc(1,16,0)
demo =
#include "demo-1-16.inc"
demochapters =
#include "demo-1-16-chapters.inc"
#else
demo =
#include "demo.inc"
demochapters =
#include "demo-chapters.inc"
#endif
