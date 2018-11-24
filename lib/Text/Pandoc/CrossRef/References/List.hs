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

module Text.Pandoc.CrossRef.References.List (listOf) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.Accessor.Monad.Trans.State
import Control.Arrow
import Data.List
import qualified Data.Map as M

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options

listOf :: Options -> [Block] -> WS [Block]
listOf opts (RawBlock (Format "latex") cmd:xs)
  | Just pfxBrace <- "\\listof{" `stripPrefix` cmd
  , (pfx, "}") <- span (/='}') pfxBrace
  = getPfxData pfx >>= fmap toList . makeList opts pfx (fromList xs)
listOf Options{outFormat=f} x | isLatexFormat f = return x
listOf opts (RawBlock fmt "\\listoffigures":xs)
  | isLaTeXRawBlockFmt fmt
  = getPfxData "fig" >>= fmap toList . makeList opts "fig" (fromList xs)
listOf opts (RawBlock fmt "\\listoftables":xs)
  | isLaTeXRawBlockFmt fmt
  = getPfxData "tbl" >>= fmap toList . makeList opts "tbl" (fromList xs)
listOf opts (RawBlock fmt "\\listoflistings":xs)
  | isLaTeXRawBlockFmt fmt
  = getPfxData "lst" >>= fmap toList . makeList opts "lst" (fromList xs)
listOf _ x = return x

getPfxData :: String -> WS RefMap
getPfxData pfx = M.filterWithKey (\k _ -> (pfx <> ":") `isPrefixOf` k) <$> get referenceData

makeList :: Options -> String -> Blocks -> M.Map String RefRec -> WS Blocks
makeList opts titlef xs refs
  = return $
      getTitleForListOf opts titlef <>
      (if chaptersDepth opts > 0
        then divWith ("", ["list"], []) (mconcat $ map itemChap refsSorted)
        else orderedList (map item refsSorted))
      <> xs
  where
    refsSorted :: [(String, RefRec)]
    refsSorted = sortBy compare' $ M.toList refs
    compare' (_,RefRec{refIndex=i}) (_,RefRec{refIndex=j}) = compare i j
    item = plain . refTitle . snd
    itemChap :: (String, RefRec) -> Blocks
    itemChap = para . uncurry (\ x x0 -> x <> space <> x0) . ((numWithChap . refIndex) &&& refTitle) . snd
    numWithChap :: Index -> Inlines
    numWithChap = chapPrefix (chapDelim opts)
