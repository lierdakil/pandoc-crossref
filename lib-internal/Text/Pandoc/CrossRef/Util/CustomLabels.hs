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
module Text.Pandoc.CrossRef.Util.CustomLabels (customLabel, customHeadingLabel) where

import qualified Data.Text as T
import Text.Numeral.Roman
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.Definition

customLabel :: Meta -> T.Text -> Int -> Maybe T.Text
customLabel meta ref i
  | refLabel <- T.takeWhile (/=':') ref
  , Just cl <- lookupMeta (refLabel <> "Labels") meta
  = mkLabel i (refLabel <> "Labels") cl
  | otherwise = Nothing

customHeadingLabel :: Meta -> Int -> Int -> Maybe T.Text
customHeadingLabel meta lvl i
  | Just cl <- getMetaList Just "secLevelLabels" meta (lvl-1)
  = mkLabel i "secLevelLabels" cl
  | otherwise = Nothing

mkLabel :: Int -> T.Text -> MetaValue -> Maybe T.Text
mkLabel i n lt
  | MetaList _ <- lt
  , Just val <- toString n <$> getList (i-1) lt
  = Just val
  | toString n lt == "arabic"
  = Nothing
  | toString n lt == "roman"
  = Just $ toRoman i
  | toString n lt == "lowercase roman"
  = Just $ T.toLower $ toRoman i
  | Just (startWith, _) <- T.uncons =<< T.stripPrefix "alpha " (toString n lt)
  = Just . T.singleton $ [startWith..] !! (i-1)
  | otherwise = error $ "Unknown numeration type: " ++ show lt
