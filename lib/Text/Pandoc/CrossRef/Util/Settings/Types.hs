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

module Text.Pandoc.CrossRef.Util.Settings.Types where

import Text.Pandoc.Definition
import qualified Data.Map as M

newtype Settings = Settings { unSettings :: Meta } deriving (Eq, Ord, Show)
newtype MetaSetting = MetaSetting MetaValue deriving (Eq, Ord, Show)

lookupSettings :: String -> Settings -> Maybe MetaValue
lookupSettings k (Settings s) = lookupMeta k s

instance Semigroup Settings where
  (Settings (Meta a)) <> (Settings (Meta b)) = Settings $ Meta $ M.unionWith merge a b

instance Monoid Settings where
  mappend = (<>)
  mempty = Settings nullMeta

merge :: MetaValue -> MetaValue -> MetaValue
merge (MetaMap m1) (MetaMap m2) = MetaMap $ M.unionWith merge m1 m2
merge x _ = x
