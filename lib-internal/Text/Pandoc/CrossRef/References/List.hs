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
import qualified Data.Map as M
import Text.Pandoc.Definition
import Text.Pandoc.Builder qualified as B

import Lens.Micro
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Generic
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Template

listOf :: Block -> Options -> References -> Maybe [Block]
listOf _ opts | isLatexFormat opts = pure Nothing
listOf (RawBlock fmt content) opts | isLaTeXRawBlockFmt fmt =
  case content of
    "\\listoffigures" -> Just <$> listOfFigures opts
    "\\listoftables" -> Just <$> listOfTables opts
    "\\listoflistings" -> Just <$> listOfListings opts
    _ -> pure Nothing
listOf _ _ = pure Nothing


listOfFigures, listOfTables, listOfListings:: Options -> References -> [Block]
listOfFigures = makeList PfxImg lofItemTemplate lofTitle
listOfTables = makeList PfxTbl lotItemTemplate lotTitle
listOfListings = makeList PfxLst lolItemTemplate lolTitle

makeList
  :: Prefix
  -> (Options -> BlockTemplate)
  -> (Options -> [Block])
  -> Options
  -> References
  -> [Block]
makeList pfx tf titlef o refsMap = do
  let
    refs = refsMap ^. refsAt pfx
    items = maybe items' pure $ mergeList Nothing [] items'
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
    itemChap ref@RefRec{..} =
      let vars = M.fromDistinctAscList
            [ ("i", numWithChap vars ref)
            , ("lt", linked refLabel title)
            , ("ri", chapPrefix chapDelim refIndex)
            , ("s", maybe mempty (chapPrefix chapDelim) refSubfigure)
            , ("suf", mempty)
            , ("t", title)
            ]
      in applyTemplate' vars $ tf o
      where
        title = replaceRefsWalk o refsMap refTitle
        Options{chapDelim} = o
    linked (Just lab) = B.toList . B.link ("#" <> lab) "" . B.fromList
    linked Nothing = id
    numWithChap vars RefRec{..} = applyTemplate' vars' $ case refSubfigure of
      Nothing -> refIndexTemplate pfx
      Just _ -> subfigureRefIndexTemplate
      where
        vars' = M.insert "i" (chapPrefix chapDelim refIndex) vars
        Options{chapDelim, refIndexTemplate, subfigureRefIndexTemplate} = o
  titlef o <> [Div ("", ["list", "list-of-" <> pfxText pfx], []) items]
