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

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B

import Data.List
import qualified Data.Map as M
import Text.Pandoc.CrossRef.References.Types as Types

defaultVarFunc :: (RefRec -> String -> Maybe MetaValue)
               -> RefRec -> String -> Maybe MetaValue
defaultVarFunc self RefRec{..} x = case x of
   "idx" -> Just $ MetaString $ show refIndex
   "i" -> Just $ MetaInlines $ B.toList refIxInl
   "t" -> Just $ MetaInlines $ B.toList refTitle
   "lvl" -> Just $ MetaString $ show refLevel
   "lbl" -> Just $ MetaString refLabel
   "pfx" -> Just $ MetaString refPfx
   _ | Just y <- stripPrefix "s." x
     , Just rs <- refScope
     -> self rs y
   _ -> case M.lookup x refAttrs of
          Just [s] -> Just $ MetaString s
          Just ss -> Just $ MetaList $ map MetaString ss
          _ -> Nothing
