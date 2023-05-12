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

{-# LANGUAGE Rank2Types, OverloadedStrings, FlexibleContexts, RecordWildCards, ViewPatterns
  , LambdaCase #-}

module Text.Pandoc.CrossRef.References.Blocks.Util where

import Control.Monad.Reader.Class
import Control.Monad.State hiding (get, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walk)

import Text.Read (readMaybe)

import Control.Applicative
import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..))

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

-- | Exactly like 'Prefix' but doesn't have 'PfxSec'. @S@ stands for "safer".
-- Sections are handled specially, see
-- "Text.Pandoc.CrossRef.References.Blocks.Header"
data SPrefix
  = SPfxImg
  | SPfxEqn
  | SPfxTbl
  | SPfxLst

toPrefix :: SPrefix -> Prefix
toPrefix = \case
  SPfxImg -> PfxImg
  SPfxEqn -> PfxEqn
  SPfxTbl -> PfxTbl
  SPfxLst -> PfxLst

replaceAttr
  :: Either T.Text T.Text -- ^ Reference id
  -> [(T.Text, T.Text)] -- ^ Attributes
  -> [Inline] -- ^ Title
  -> SPrefix -- ^ Prefix type
  -> WS [Inline]
replaceAttr label attrs title (toPrefix -> pfx) = do
  let refLabel = lookup "label" attrs
      number = readMaybe . T.unpack =<< lookup "number" attrs
  Options{..} <- ask
  chap  <- S.take chaptersDepth <$> use (ctrsAt PfxSec)
  prop' <- use $ refsAt pfx
  curIdx <- use $ ctrsAt pfx
  let i | Just n <- number = n
        | chap' :> last' <- S.viewr curIdx
        , chap' == chap
        = succ . fst $ last'
        | otherwise = 1
      index = chap S.|> (i, refLabel <|> customLabel ref i)
      ref = either id (T.takeWhile (/=':')) label
      label' = either (<> T.pack (':' : show index)) id label
  when (M.member label' prop') $
    error . T.unpack $ "Duplicate label: " <> label'
  ctrsAt pfx .= index
  modifying (refsAt pfx) $ M.insert label' RefRec {
    refIndex= index
  , refTitle= title
  , refSubfigure = Nothing
  }
  return $ chapPrefix chapDelim index

mkCaption :: Options -> T.Text -> [Inline] -> Block
mkCaption opts style
  | outFormat opts == Just (Format "docx") = Div ("", [], [("custom-style", style)]) . return . Para
  | otherwise = Para
