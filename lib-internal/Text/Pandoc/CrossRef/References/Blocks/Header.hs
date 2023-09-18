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

{-# LANGUAGE Rank2Types, OverloadedStrings, FlexibleContexts, RecordWildCards, NamedFieldPuns #-}

module Text.Pandoc.CrossRef.References.Blocks.Header where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State hiding (get, modify)
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..))
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Read (readMaybe)

import Control.Applicative
import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util (setLabel)
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

runHeader :: Int -> Attr -> [Inline] -> WS (ReplacedResult Block)
runHeader n (label, cls, attrs) text'
  | "unnumbered" `elem` cls = do
      label' <- mangleLabel
      replaceNoRecurse $ Header n (label', cls, attrs) text'
  | otherwise = do
      opts@Options{..} <- ask
      label' <- mangleLabel
      ctrsAt PfxSec %= \cc ->
        let ln = length cc
            cl i = lookup "label" attrs <|> customHeadingLabel n i <|> customLabel "sec" i
            inc l = case S.viewr l of
              EmptyR -> error "impossible"
              init' :> last' ->
                let i = succ $ fst $ last'
                in init' S.|> (i, cl i)
            cc' | Just num <- readMaybe . T.unpack =<< lookup "number" attrs
                = S.take (n - 1) cc S.|> (num, cl num)
                | ln > n = inc $ S.take n cc
                | ln == n = inc cc
                | otherwise = cc <> implicitChapters S.|> (1,cl 1)
            implicitChapters
              | numberSections = S.replicate (n-ln-1) (1, Nothing)
              | otherwise = S.replicate (n-ln-1) (0, Nothing)
        in cc'
      cc <- use $ ctrsAt PfxSec
      when ("sec:" `T.isPrefixOf` label') $
        refsAt PfxSec %= M.insert label' RefRec {
          refIndex = cc
        , refTitle = text'
        , refSubfigure = Nothing
        }
      let textCC
            | numberSections
            , sectionsDepth < 0
            || n <= if sectionsDepth == 0 then chaptersDepth else sectionsDepth
            = applyTemplate' (M.fromDistinctAscList [
                ("i", idxStr)
              , ("n", [Str $ T.pack $ show $ n - 1])
              , ("t", text')
              ]) $ secHeaderTemplate
            | otherwise = text'
          idxStr = chapPrefix chapDelim cc
          attrs' = setLabel opts idxStr attrs
      replaceNoRecurse $ Header n (label', cls, attrs') textCC
  where
    mangleLabel = do
      Options{autoSectionLabels} <- ask
      pure $
        if autoSectionLabels && not ("sec:" `T.isPrefixOf` label)
        then "sec:" <> label
        else label
