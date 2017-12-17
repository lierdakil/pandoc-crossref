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

module Text.Pandoc.CrossRef.Util.CustomLabels where

import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Meta
import Data.List
import Text.Numeral.Roman

mkLabel :: String -> MetaValue -> Int -> String
mkLabel n lt i
  | toString n lt == "arabic"
  = show i
  | toString n lt == "roman"
  = toRoman i
  | Just (startWith:_) <- stripPrefix "alpha " $ toString n lt
  = [[startWith..] !! (i-1)]
  | Just val <- toString n <$> getList (i-1) lt
  = val
  | otherwise = error $ "Unknown numeration type: " ++ show lt
