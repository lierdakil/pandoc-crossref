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

{-# LANGUAGE NamedFieldPuns #-}

module Text.Pandoc.CrossRef.References.List (listOf) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.List
import Data.Function
import qualified Data.Map as M

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Prefixes.Types
import Text.Pandoc.CrossRef.Util.VarFunction

listOf :: Options -> [Block] -> WS [Block]
listOf opts (RawBlock fmt cmd:xs)
  | isLaTeXRawBlockFmt fmt
  , Just pfxBrace <- "\\listof{" `stripPrefix` cmd
  , (pfx, "}") <- span (/='}') pfxBrace
  = getPfxData pfx >>= fmap toList . makeList opts pfx (fromList xs)
listOf _ x = return x

getPfxData :: String -> WS RefMap
getPfxData pfx = M.filterWithKey (\k _ -> (pfx <> ":") `isPrefixOf` k) <$> get referenceData

makeList :: Options -> String -> Blocks -> M.Map String RefRec -> WS Blocks
makeList opts titlef xs refs
  = do
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
