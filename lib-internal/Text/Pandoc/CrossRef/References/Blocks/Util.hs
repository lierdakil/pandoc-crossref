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

module Text.Pandoc.CrossRef.References.Blocks.Util where

import Control.Monad.Reader.Class
import Control.Monad.State hiding (get, modify)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walk)

import Control.Applicative
import Lens.Micro
import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util

setLabel :: Options -> [Inline] -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
setLabel opts idx
  | setLabelAttribute opts
  = (("label", stringify idx) :)
  . filter ((/= "label") . fst)
  | otherwise = id

walkReplaceInlines :: [Inline] -> [Inline] -> Block -> Block
walkReplaceInlines newTitle title = walk replaceInlines
  where
  replaceInlines xs
    | xs == title = newTitle
    | otherwise = xs

replaceAttr :: Either T.Text T.Text -> Maybe T.Text -> [Inline] -> Lens References References RefMap RefMap -> WS [Inline]
replaceAttr label refLabel title prop
  = do
    o <- ask
    chap  <- take (chaptersDepth o) `fmap` use curChap
    prop' <- use prop
    let i = 1+ (M.size . M.filter (\x -> (chap == init (refIndex x)) && isNothing (refSubfigure x)) $ prop')
        index = chap <> [(i, refLabel <|> customLabel o ref i)]
        ref = either id (T.takeWhile (/=':')) label
        label' = either (<> T.pack (':' : show index)) id label
    when (M.member label' prop') $
      error . T.unpack $ "Duplicate label: " <> label'
    modifying prop $ M.insert label' RefRec {
      refIndex= index
    , refTitle= title
    , refSubfigure = Nothing
    }
    return $ chapPrefix (chapDelim o) index

mkCaption :: Options -> T.Text -> [Inline] -> Block
mkCaption opts style
  | outFormat opts == Just (Format "docx") = Div ("", [], [("custom-style", style)]) . return . Para
  | otherwise = Para
