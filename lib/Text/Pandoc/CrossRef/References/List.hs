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
listOf Options{outFormat=f} x | isFormat "latex" f = return x
listOf opts (RawBlock (Format "latex") "\\listoffigures":xs)
  = undefined -- get imgRefs >>= makeList opts lofTitle xs
listOf opts (RawBlock (Format "latex") "\\listoftables":xs)
  = undefined -- get tblRefs >>= makeList opts lotTitle xs
listOf opts (RawBlock (Format "latex") "\\listoflistings":xs)
  = undefined -- get lstRefs >>= makeList opts lolTitle xs
listOf _ x = return x

makeList :: Options -> (Options -> Blocks) -> Blocks -> M.Map String RefRec -> WS Blocks
makeList opts titlef xs refs
  = return $
      titlef opts <>
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
