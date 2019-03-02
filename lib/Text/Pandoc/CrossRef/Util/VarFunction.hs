{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2019  Nikolay Yakimov <root@livid.pp.ru>

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

{-# LANGUAGE RecordWildCards #-}

module Text.Pandoc.CrossRef.Util.VarFunction where

import qualified Text.Pandoc.Builder as B

import Data.List
import Text.Pandoc.CrossRef.References.Types as Types

defaultVarFunc :: (RefRec -> String -> Maybe B.Inlines)
               -> RefRec -> String -> Maybe B.Inlines
defaultVarFunc self RefRec{..} x = case x of
   "idx" -> Just $ B.str $ show refIndex
   "i" -> Just refIxInl
   "t" -> Just refTitle
   "lvl" -> Just $ B.str $ show refLevel
   "lbl" -> Just $ B.str refLabel
   "pfx" -> Just $ B.str refPfx
   _ | Just y <- stripPrefix "s." x
     , Just rs <- refScope
     -> self rs y
   _ -> Nothing
