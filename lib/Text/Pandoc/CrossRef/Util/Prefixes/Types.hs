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

{-# LANGUAGE Rank2Types, TypeFamilies #-}

module Text.Pandoc.CrossRef.Util.Prefixes.Types where

import qualified Data.Map as M
import Text.Pandoc.CrossRef.Util.Template.Types
import Text.Pandoc.Builder

type Prefixes = M.Map String Prefix

data Prefix = Prefix {
    prefixCaptionTemplate :: !Template
  , prefixReferenceIndexTemplate :: !Template
  , prefixCaptionIndexTemplate :: !Template
  , prefixListItemTemplate :: !Template
  , prefixCollectedCaptionTemplate :: !Template
  , prefixReferenceTemplate :: !RefTemplate
  , prefixListOfTitle :: !BlockTemplate
  , prefixCollectedCaptionDelim :: !Inlines
  , prefixScope :: ![String]
  , prefixNumbering :: !(Int -> String)
  , prefixSubcaptions :: !Bool
  , prefixSubcaptionsGrid :: !Bool
  , prefixSub :: !(Maybe Prefix)
}
