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

{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.CrossRef.Util.Options (
    module Text.Pandoc.CrossRef.Util.Options
  , module Text.Pandoc.CrossRef.Util.Options.Types
) where

import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Options.Types
import Text.Pandoc.CrossRef.References.Types.Monad
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Prefixes
import qualified Data.Map as M
import Text.Pandoc.Builder
import qualified Data.Text as T

prefixList :: Options -> [T.Text]
prefixList = M.keys . prefixes

pfxCaptionTemplate :: Options -> T.Text -> PureErr Template
pfxCaptionTemplate opts pfx = prefixCaptionTemplate <$> getPfx opts pfx

pfxListItemTemplate :: Options -> T.Text -> PureErr Template
pfxListItemTemplate opts pfx = prefixListItemTemplate <$> getPfx opts pfx

pfxCaptionIndexTemplate :: Options -> T.Text -> PureErr Template
pfxCaptionIndexTemplate opts pfx = prefixCaptionIndexTemplate <$> getPfx opts pfx

getPfx :: Options -> T.Text -> PureErr Prefix
getPfx o pn = maybe defaultPfx return $ M.lookup pn $ prefixes o
  where
    defaultPfx = Left $ WSENoSuchPrefix pn

getRefPrefix :: Options -> T.Text -> Maybe T.Text
getRefPrefix opts label
  | (pfx, rest) <- T.span (/=':') label
  , not $ T.null rest
  = if pfx `elem` prefixList opts
    then Just pfx
    else Nothing
  | otherwise = Nothing

getRefLabel :: Options -> [Inline] -> Maybe T.Text
getRefLabel _ [] = Nothing
getRefLabel opts ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , Just lbl <- T.stripPrefix "{#" attr >>= T.stripSuffix "}"
  , Just _ <- getRefPrefix opts lbl
  = Just lbl
getRefLabel _ _ = Nothing

getTitleForListOf :: Options -> T.Text -> PureErr Blocks
getTitleForListOf opts pfxn = do
  pfx <- getPfx opts pfxn
  return $ applyBlockTemplate (prefixListOfTitle pfx) (prefixDef pfx)
