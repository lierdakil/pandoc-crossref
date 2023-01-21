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

{-# LANGUAGE Rank2Types, OverloadedStrings, FlexibleContexts #-}

module Text.Pandoc.CrossRef.References.Blocks.Header where

import Control.Monad.Reader.Class
import Control.Monad.State hiding (get, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition

import Control.Applicative
import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util (setLabel)
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

runHeader :: Int -> Attr -> [Inline] -> WS (ReplacedResult Block)
runHeader n (label, cls, attrs) text' = do
  opts <- ask
  let label' = if autoSectionLabels opts && not ("sec:" `T.isPrefixOf` label)
                then "sec:"<>label
                else label
  unless ("unnumbered" `elem` cls) $ do
    modifying curChap $ \cc ->
      let ln = length cc
          cl i = lookup "label" attrs <|> customHeadingLabel opts n i <|> customLabel opts "sec" i
          inc l = let i = fst (last l) + 1 in init l <> [(i, cl i)]
          cc' | ln > n = inc $ take n cc
              | ln == n = inc cc
              | otherwise = cc <> take (n-ln-1) implicitChapters <> [(1,cl 1)]
          implicitChapters | numberSections opts = repeat (1, Nothing)
                            | otherwise = repeat (0, Nothing)
      in cc'
    when ("sec:" `T.isPrefixOf` label') $ do
      index  <- use curChap
      modifying secRefs $ M.insert label' RefRec {
        refIndex=index
      , refTitle= text'
      , refSubfigure = Nothing
      }
  cc <- use curChap
  let textCC | numberSections opts
              , sectionsDepth opts < 0
              || n <= if sectionsDepth opts == 0 then chaptersDepth opts else sectionsDepth opts
              , "unnumbered" `notElem` cls
              = applyTemplate' (M.fromDistinctAscList [
                  ("i", idxStr)
                , ("n", [Str $ T.pack $ show $ n - 1])
                , ("t", text')
                ]) $ secHeaderTemplate opts
              | otherwise = text'
      idxStr = chapPrefix (chapDelim opts) cc
      attrs' | "unnumbered" `notElem` cls
              = setLabel opts idxStr attrs
              | otherwise = attrs
  replaceNoRecurse $ Header n (label', cls, attrs') textCC
