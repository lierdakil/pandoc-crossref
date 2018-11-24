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

{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Text.Pandoc.CrossRef.Util.Prefixes where

import Text.Pandoc.Definition
import Text.Pandoc.Walk (Walkable(..))
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.CustomLabels
import qualified Data.Map as M
import Text.Pandoc.Builder hiding ((<>))
import Data.Default
import Data.Maybe

newtype Inlines' = Inlines' { fromInlines' :: Inlines } deriving (Eq, Semigroup, Monoid)
instance Default Inlines' where
  def = mempty
instance Walkable Inline Inlines' where
  walk f (Inlines' x) = Inlines' $ walk f x
  walkM f (Inlines' x) = Inlines' <$> walkM f x
  query f (Inlines' x) = query f x


getPrefixes :: String -> Settings -> Prefixes
getPrefixes varN dtv
  | Just (MetaMap m) <- lookupSettings varN dtv = M.mapWithKey m2p m
  | otherwise = error "Prefixes not defined"
  where
    var = displayMath
    m2p _ (MetaMap kv') = Prefix {
        prefixRef = (fromInlines' .) . tryCapitalizeM (flip (getMetaList (Inlines' . toInlines "ref")) kv) "ref"
      , prefixCaptionTemplate = makeTemplate kv $
          if isJust $ lookupSettings "captionTemplate" kv
          then getMetaInlines "captionTemplate" kv
          else var "title" <> space <> var "i" <> var "titleDelim" <> space <> var "t" <> var "ccs#. "
      , prefixReferenceTemplate = makeTemplate kv $
          if isJust $ lookupSettings "referenceTemplate" kv
          then getMetaInlines "referenceTemplate" kv
          else var "p" <> str "\160" <> var "i"
      , prefixScope = getMetaStringMaybe "scope" kv
      , prefixNumbering = mkLabel (varN <> "." <> "numbering") (fromMaybe (MetaString "arabic") $ lookupSettings "numbering" kv)
      , prefixListOfTitle = getMetaBlock "listOfTitle" kv
      , prefixTitle = getMetaInlines "title" kv
      }
      where kv = Settings (Meta kv') <> dtv
    m2p k _ = error $ "Invalid value for prefix " <> k

type Prefixes = M.Map String Prefix

data Prefix = Prefix {
    prefixRef :: !(Bool -> Int -> Inlines)
  , prefixCaptionTemplate :: !Template
  , prefixReferenceTemplate :: !Template
  , prefixScope :: !(Maybe String)
  , prefixNumbering :: !(Int -> String)
  , prefixListOfTitle :: !Blocks
  -- Used for LaTeX metadata; the same value is used in
  -- default value for prefixCaptionTemplate
  , prefixTitle :: Inlines
}
