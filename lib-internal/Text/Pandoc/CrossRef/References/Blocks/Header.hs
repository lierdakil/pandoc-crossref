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

module Text.Pandoc.CrossRef.References.Blocks.Header where

import Control.Applicative
import Lens.Micro.Mtl
import Control.Monad (when)
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..))
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Read (readMaybe)

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util (setLabel, checkHidden)
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Generic

runHeader :: Int -> Attr -> [Inline] -> WS (ReplacedResult Block)
runHeader n (label, cls, attrs) text' = do
  wsReferences . stHiddenHeaderLevel %= filter \HiddenHeader{hhLevel} -> hhLevel < n
  hhHidden <- checkHidden attrs
  wsReferences . stHiddenHeaderLevel %= (HiddenHeader { hhLevel = n, hhHidden }:)

  if "unnumbered" `elem` cls
    then do
      label' <- mangleLabel
      replaceRecurse $ Header n (label', cls, attrs) text'
    else do
      opts@Options{..} <- use wsOptions
      label' <- mangleLabel
      wsReferences . ctrsAt PfxSec %= \cc ->
        let ln = length cc
            cl i = lookup "label" attrs <|> customHeadingLabel n i <|> customLabel (Pfx PfxSec) i
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
      cc <- use $ wsReferences . ctrsAt PfxSec
      globCtr <- wsReferences . stGlob <<%= (+ 1)
      when (label' `hasPfx` PfxSec) $
        wsReferences . refsAt PfxSec %= M.insert label' RefRec {
          refIndex = cc
        , refGlobal = globCtr
        , refTitle = text'
        , refSubfigure = Nothing
        , refHideFromList = hhHidden
        , refLabel = if T.null label' then Nothing else Just label'
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
      replaceRecurse $ Header n (label', cls, attrs') textCC
  where
    mangleLabel = do
      Options{autoSectionLabels} <- use wsOptions
      pure $
        if autoSectionLabels && not (label `hasPfx` PfxSec)
        then pfxTextCol PfxSec <> label
        else label
