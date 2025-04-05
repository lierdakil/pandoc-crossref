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

module Text.Pandoc.CrossRef.References.List (listOf, listOfFigures, listOfTables, listOfListings) where

import Data.List
import Control.Monad.Reader
import qualified Data.Map as M
import Text.Pandoc.Definition
import Data.Maybe

import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Template

listOf :: [Block] -> WS [Block]
listOf blocks = asks isLatexFormat >>= \case
  True -> pure blocks
  False -> case blocks of
    (RawBlock fmt "\\listoffigures":xs)
      | isLaTeXRawBlockFmt fmt
      -> (<> xs) <$> listOfFigures
    (RawBlock fmt "\\listoftables":xs)
      | isLaTeXRawBlockFmt fmt
      -> (<> xs) <$> listOfTables
    (RawBlock fmt "\\listoflistings":xs)
      | isLaTeXRawBlockFmt fmt
      -> (<> xs) <$> listOfListings
    _ -> pure blocks

listOfFigures, listOfTables, listOfListings:: WS [Block]
listOfFigures = makeList PfxImg lofItemTemplate lofTitle
listOfTables = makeList PfxTbl lotItemTemplate lotTitle
listOfListings = makeList PfxLst lolItemTemplate lolTitle

makeList
  :: Prefix
  -> (Options -> BlockTemplate)
  -> (Options -> [Block])
  -> WS [Block]
makeList pfx tf titlef = do
  o <- ask
  refs <- use $ refsAt pfx
  let
    items = fromMaybe items' $ pure <$> mergeList Nothing [] items'
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
    refsSorted = sortBy compare' $ filter notHidden $ M.toList refs
    notHidden (_, RefRec{refHideFromList}) = not refHideFromList
    compare'
      (_,RefRec{refGlobal=i})
      (_,RefRec{refGlobal=j})
      = compare i j
    itemChap :: RefRec -> [Block]
    itemChap ref@RefRec{..} = applyTemplate (numWithChap ref) refTitle (tf o)
    numWithChap :: RefRec -> [Inline]
    numWithChap RefRec{..} = case refSubfigure of
      Nothing ->
        let vars = M.fromDistinctAscList
              [ ("i", chapPrefix chapDelim refIndex)
              , ("suf", mempty)
              , ("t", refTitle)
              ]
        in applyTemplate' vars $ refIndexTemplate pfx
      Just s ->
        let vars = M.fromDistinctAscList
              [ ("i", chapPrefix chapDelim refIndex)
              , ("s", chapPrefix chapDelim s)
              , ("suf", mempty)
              , ("t", refTitle)
              ]
        in applyTemplate' vars subfigureRefIndexTemplate
      where
        Options{chapDelim, refIndexTemplate, subfigureRefIndexTemplate} = o
  pure $ titlef o <> [Div ("", ["list", "list-of-" <> pfxText pfx], []) items]
