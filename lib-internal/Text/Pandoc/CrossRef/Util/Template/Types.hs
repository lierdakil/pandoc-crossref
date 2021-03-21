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

{-# LANGUAGE TypeFamilies #-}

module Text.Pandoc.CrossRef.Util.Template.Types where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import qualified Data.Text as T

type VarFunc = T.Text -> Maybe MetaValue
newtype Template = Template { applyTemplate :: VarFunc -> Inlines }
newtype RefTemplate = RefTemplate { applyRefTemplate :: VarFunc -> Bool -> Inlines }
newtype BlockTemplate = BlockTemplate { applyBlockTemplate :: VarFunc -> Blocks }

class MakeTemplate a where
  type ElemT a
  makeTemplate :: ElemT a -> a
