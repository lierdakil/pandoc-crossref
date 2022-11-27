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

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Text.Pandoc.CrossRef.References.List (listOf) where

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Data.Maybe

import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Template

listOf :: Options -> [Block] -> WS [Block]
listOf Options{outFormat=f} x | isLatexFormat f = return x
listOf opts (RawBlock fmt "\\listoffigures":xs)
  | isLaTeXRawBlockFmt fmt
  = use imgRefs >>= makeList "fig" opts lofItemTemplate lofTitle xs
listOf opts (RawBlock fmt "\\listoftables":xs)
  | isLaTeXRawBlockFmt fmt
  = use tblRefs >>= makeList "tbl" opts lotItemTemplate lotTitle xs
listOf opts (RawBlock fmt "\\listoflistings":xs)
  | isLaTeXRawBlockFmt fmt
  = use lstRefs >>= makeList "lst" opts lolItemTemplate lolTitle xs
listOf _ x = return x

makeList
  :: T.Text
  -> Options
  -> (Options
  -> BlockTemplate)
  -> (Options
  -> [Block])
  -> [Block]
  -> M.Map T.Text RefRec
  -> WS [Block]
makeList pfx o tf titlef xs refs =
  pure $ titlef o <> (Div ("", ["list", "list-of-" <> pfx], []) items : xs)
  where
    items = fromMaybe items'$ pure <$> mergeList Nothing [] items'
    items' = concatMap (itemChap . snd) refsSorted
    mergeList Nothing acc (OrderedList style item : ys) =
      mergeList (Just $ OrderedList style) (item <> acc) ys
    mergeList Nothing acc (BulletList item : ys) =
      mergeList (Just BulletList) (item <> acc) ys
    mergeList (Just cons) acc (OrderedList style item : ys)
      | cons [] == OrderedList style [] =
      mergeList (Just $ OrderedList style) (item <> acc) ys
    mergeList (Just cons) acc (BulletList item : ys)
      | cons [] == BulletList[] =
      mergeList (Just BulletList) (item <> acc) ys
    mergeList (Just cons) acc [] = Just $ cons $ reverse acc
    mergeList _ _ _ = Nothing
    refsSorted = sortBy compare' $ M.toList refs
    compare'
      (_,RefRec{refIndex=i, refSubfigure=si})
      (_,RefRec{refIndex=j, refSubfigure=sj})
      = compare (i, si) (j, sj)
    itemChap :: RefRec -> [Block]
    itemChap ref@RefRec{..} = applyTemplate (numWithChap ref) refTitle (tf o)
    numWithChap :: RefRec -> [Inline]
    numWithChap RefRec{..} = case refSubfigure of
      Nothing ->
        let vars = M.fromDistinctAscList
              [ ("i", chapPrefix (chapDelim o) refIndex)
              , ("suf", mempty)
              , ("t", refTitle)
              ]
        in applyTemplate' vars $ refIndexTemplate o pfx
      Just s ->
        let vars = M.fromDistinctAscList
              [ ("i", chapPrefix (chapDelim o) refIndex)
              , ("s", chapPrefix (chapDelim o) s)
              , ("suf", mempty)
              , ("t", refTitle)
              ]
        in applyTemplate' vars $ subfigureRefIndexTemplate o
