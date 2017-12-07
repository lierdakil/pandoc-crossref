{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module Text.Pandoc.CrossRef.Util.Template
  ( Template
  , makeTemplate
  , applyTemplate
  , applyTemplate'
  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Generic
import Data.Map as M hiding (toList, fromList, singleton)
import Text.Pandoc.CrossRef.Util.Meta
import Control.Applicative

type VarFunc = String -> Maybe MetaValue
newtype Template = Template (VarFunc -> [Inline])

makeTemplate :: Meta -> [Inline] -> Template
makeTemplate dtv xs' = Template $ \vf -> scan (\var -> vf var <|> lookupMeta var dtv) xs'
  where
  scan = bottomUp . go
  go vf (x@(Math DisplayMath var):xs) = toList $ fromList (replaceVar var (vf var) [x]) <> fromList xs
  go _ (x:xs) = toList $ singleton x <> fromList xs
  go _ [] = []
  replaceVar var val def' = maybe def' (toInlines ("variable " ++ var)) val

applyTemplate' :: Map String [Inline] -> Template -> [Inline]
applyTemplate' vars (Template g) = g internalVars
  where
  internalVars x | Just v <- M.lookup x vars = Just $ MetaInlines v
  internalVars _   = Nothing

applyTemplate :: [Inline] -> [Inline] -> Template -> [Inline]
applyTemplate i t =
  applyTemplate' (fromDistinctAscList [("i", i), ("t", t)])
