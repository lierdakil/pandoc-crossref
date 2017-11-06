module Text.Pandoc.CrossRef.Util.Template
  ( Template
  , makeTemplate
  , applyTemplate
  , applyTemplate'
  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Data.Maybe
import Data.Map as M
import Text.Pandoc.CrossRef.Util.Meta

type VarFunc = String -> Maybe MetaValue
newtype Template = Template (VarFunc -> [Inline])

makeTemplate :: Meta -> [Inline] -> Template
makeTemplate dtv = Template . flip scan . scan (`lookupMeta` dtv)
  where
  scan = bottomUp . go
  go vf (x@(Math DisplayMath var):xs) = replaceVar var (vf var) [x] ++ xs
  go _ x = x
  replaceVar var val def' = fromMaybe def' $ val >>= toInlines ("variable " ++ var)

applyTemplate' :: Map String [Inline] -> Template -> [Inline]
applyTemplate' vars (Template g) = g internalVars
  where
  internalVars x | Just v <- M.lookup x vars = Just $ MetaInlines v
  internalVars _   = Nothing

applyTemplate :: [Inline] -> [Inline] -> Template -> [Inline]
applyTemplate i t =
  applyTemplate' (fromDistinctAscList [("i", i), ("t", t)])
