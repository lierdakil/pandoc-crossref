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

{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Text.Pandoc.CrossRef.References.List (listOf) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.List
import Data.Function
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Prefixes.Types
import Text.Pandoc.CrossRef.Util.VarFunction

listOf :: [Block] -> WS [Block]
listOf (RawBlock fmt cmd:xs)
  | isLaTeXRawBlockFmt fmt
  , Just pfxBrace <- "\\listof{" `T.stripPrefix` cmd
  , (pfx, "}") <- T.span (/='}') pfxBrace
  = getPfxData pfx >>= fmap toList . makeList pfx (fromList xs)
listOf x = return x

getPfxData :: T.Text -> WS RefMap
getPfxData pfx = M.filterWithKey (\k _ -> (pfx <> ":") `T.isPrefixOf` k) <$> get referenceData

makeList :: T.Text -> Blocks -> M.Map T.Text RefRec -> WS Blocks
makeList titlef xs refs
  = do
    opts <- asks creOptions
    title <- liftEither $ getTitleForListOf opts titlef
    return $ title
      <> divWith ("", ["list"], []) (mconcat $ map itemChap refsSorted)
      <> xs
  where
    refsSorted :: [RefRec]
    refsSorted = sortBy (compare `on` refIndex) $ M.elems refs
    itemChap :: RefRec -> Blocks
    itemChap = para . applyListItemTemplate

applyListItemTemplate :: RefRec -> Inlines
applyListItemTemplate rr@RefRec{refPfxRec} =
  applyTemplate (prefixListItemTemplate refPfxRec) (fix defaultVarFunc rr)
