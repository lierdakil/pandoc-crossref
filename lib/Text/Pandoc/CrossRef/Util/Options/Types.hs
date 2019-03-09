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

module Text.Pandoc.CrossRef.Util.Options.Types where
import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Prefixes
import Text.Pandoc.Builder

data Options = Options { codeBlockCaptions  :: Bool
                       , autoSectionLabels  :: Maybe String
                       , defaultSectionPrefix :: String
                       , rangeDelim  :: Inlines
                       , pairDelim  :: Inlines
                       , lastDelim  :: Inlines
                       , refDelim  :: Inlines
                       , outFormat   :: Maybe Format
                       , tableEqns :: Bool
                       , autoEqnLabels :: Maybe String
                       , autoFigLabels :: Maybe String
                       , linkReferences :: Bool
                       , nameInLink :: Bool
                       , prefixes :: Prefixes
                       }
