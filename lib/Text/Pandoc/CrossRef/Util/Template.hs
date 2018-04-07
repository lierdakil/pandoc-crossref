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
import qualified Data.Map as M hiding (toList, fromList, singleton)
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.Settings.Types
import Control.Applicative
import Text.Read

type VarFunc = String -> Maybe MetaValue
newtype Template = Template (VarFunc -> Inlines)

makeTemplate :: Settings -> Inlines -> Template
makeTemplate dtv xs' = Template $ \vf -> fromList $ scan (\var -> vf var <|> lookupSettings var dtv) $ toList xs'
  where
  scan :: (String -> Maybe MetaValue) -> [Inline] -> [Inline]
  scan = bottomUp . go
  go vf ((Math DisplayMath var):xs)
    | '[' `elem` var  && ']' == last var =
      let (vn, idxBr) = span (/='[') var
          idxVar = drop 1 $ takeWhile (/=']') idxBr
          idx = readMaybe . toString ("index variable " ++ idxVar) =<< (vf idxVar)
          arr = do
            i <- idx
            v <- lookupSettings vn dtv
            getList i v
      in toList $ (replaceVar arr id) <> fromList xs
    | '#' `elem` var =
      let (vn, pfx') = span (/='#') var
          pfx = drop 1 pfx'
      in toList $ (replaceVar (vf vn) (text pfx <>)) <> fromList xs
    | otherwise = toList $ (replaceVar (vf var) id) <> fromList xs
    where replaceVar val m = maybe mempty (m . toInlines ("variable " ++ var)) val
  go _ (x:xs) = toList $ singleton x <> fromList xs
  go _ [] = []

applyTemplate' :: M.Map String Inlines -> Template -> Inlines
applyTemplate' vars (Template g) = g internalVars
  where
  internalVars x | Just v <- M.lookup x vars = Just $ MetaInlines $ toList v
  internalVars _   = Nothing

applyTemplate :: Inlines -> Inlines -> Template -> Inlines
applyTemplate i t =
  applyTemplate' (M.fromDistinctAscList [("i", i), ("t", t)])
