{-# LANGUAGE RecordWildCards #-}
module Text.Pandoc.CrossRef.References.Accessors where

import Text.Pandoc.CrossRef.References.Types
import Data.Accessor

imgRefs' :: Accessor References RefMap
imgRefs' = accessor
  (\References{..} -> imgRefs)
  (\a r -> r{imgRefs=a})

eqnRefs' :: Accessor References RefMap
eqnRefs' = accessor
  (\References{..} -> eqnRefs)
  (\a r -> r{eqnRefs=a})

tblRefs' :: Accessor References RefMap
tblRefs' = accessor
  (\References{..} -> tblRefs)
  (\a r -> r{tblRefs=a})

lstRefs' :: Accessor References RefMap
lstRefs' = accessor
  (\References{..} -> lstRefs)
  (\a r -> r{lstRefs=a})

secRefs' :: Accessor References RefMap
secRefs' = accessor
  (\References{..} -> secRefs)
  (\a r -> r{secRefs=a})

curChap' :: Accessor References Index
curChap' = accessor
  (\References{..} -> curChap)
  (\a r -> r{curChap=a})
