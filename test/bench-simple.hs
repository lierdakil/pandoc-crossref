{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
import Criterion.Main
import Text.Pandoc.CrossRef
import Text.Pandoc
import Native

main :: IO ()
main = defaultMain [
    bench "demo" (nf go demo)
  , bench "demoChapters" (nf go demochapters)
  ]
  where
    go = runCrossRef nullMeta Nothing crossRefBlocks
  -- map (\l -> bench ("concat dlist " <> show l) . nf (uncurry append) $ dlists l) lengths
