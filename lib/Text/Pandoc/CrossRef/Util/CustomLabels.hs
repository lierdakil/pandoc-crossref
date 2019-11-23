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
module Text.Pandoc.CrossRef.Util.CustomLabels where

import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Meta
import Text.Numeral.Roman
import qualified Data.Text as T

mkLabel :: T.Text -> MetaValue -> Int -> T.Text
mkLabel n lt i
  | MetaList _ <- lt
  , Just val <- toString n <$> getList (i-1) lt
  = val
  | toString n lt == "arabic"
  = T.pack $ show i
  | toString n lt == "roman"
  = toRoman i
  | Just (startWith,_) <- T.uncons =<< T.stripPrefix "alpha " (toString n lt)
  = T.singleton . toEnum $ fromEnum startWith + i - 1
  | otherwise = error $ "Unknown numeration type: " ++ show lt
