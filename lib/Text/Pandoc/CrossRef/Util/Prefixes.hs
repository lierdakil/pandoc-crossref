{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2017  Nikolay Yakimov <root@livid.pp.ru>

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

{-# LANGUAGE FlexibleInstances #-}
module Text.Pandoc.CrossRef.Util.Prefixes where

import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.CustomLabels
import qualified Data.Map as M
import Text.Pandoc.Builder
import Data.Default
import Data.Maybe

instance Default Inlines where
  def = mempty

getPrefixes :: String -> Meta -> Prefixes
getPrefixes varN dtv
  | Just (MetaMap m) <- lookupMeta varN dtv = M.mapWithKey m2p m
  | otherwise = error "Prefixes not defined"
  where
    m2p _ (MetaMap kv') = Prefix {
        prefixRef = tryCapitalizeM (flip (getMetaList (toInlines "ref")) kv) "ref"
      , prefixTitle = getMetaInlines "title" kv
      , prefixCaptionTemplate = makeTemplate kv $ getMetaInlines "captionTemplate" kv
      , prefixReferenceTemplate = makeTemplate kv $ getMetaInlines "referenceTemplate" kv
      , prefixScope = getMetaStringMaybe "scope" kv
      , prefixNumbering = mkLabel (varN <> "." <> "numbering") (fromMaybe (error "...") $ lookupMeta "numbering" kv)
      , prefixListOfTitle = getMetaBlock "listOfTitle" kv
      }
      where kv = Meta kv'
    m2p k _ = error $ "Invalid value for prefix " <> k

type Prefixes = M.Map String Prefix

data Prefix = Prefix {
    prefixRef :: !(Bool -> Int -> Inlines)
  , prefixTitle :: !Inlines
  , prefixCaptionTemplate :: !Template
  , prefixReferenceTemplate :: !Template
  , prefixScope :: !(Maybe String)
  , prefixNumbering :: !(Int -> String)
  , prefixListOfTitle :: !Blocks
}
