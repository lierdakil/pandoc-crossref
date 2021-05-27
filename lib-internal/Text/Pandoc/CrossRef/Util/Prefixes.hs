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

{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.CrossRef.Util.Prefixes (
    getPrefixes
  , module Types
) where

import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Prefixes.Types as Types
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.CustomLabels
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T

getPrefixes :: Maybe Format -> T.Text -> Settings -> Prefixes
getPrefixes fmt varN dtv
  | Just (MetaMap m) <- lookupSettings varN dtv =
    let
      m2p :: T.Text -> MetaValue -> Prefix
      m2p k (MetaMap kv') = Prefix {
          prefixCaptionTemplate = makeTemplate $ getTemplInline "captionTemplate"
        , prefixReferenceTemplate = makeTemplate $ getTemplInline "referenceTemplate"
        , prefixReferenceIndexTemplate = makeTemplate $ getTemplInline "referenceIndexTemplate"
        , prefixCaptionIndexTemplate = makeTemplate $ getTemplInline "captionIndexTemplate"
        , prefixListItemTemplate = makeTemplate $ getTemplInline "listItemTemplate"
        , prefixCollectedCaptionTemplate = makeTemplate $ getTemplInline "collectedCaptionTemplate"
        , prefixListOfTitle = makeTemplate $ getTemplBlock "listOfTitle"
        , prefixCollectedCaptionDelim = getMetaInlines "collectedCaptionDelim" kv
        , prefixScope = getMetaStringList "scope" kv
        , prefixNumbering =
            let prettyVarName = varN <> "." <> k <> "." <> varName
                varName = "numbering"
            in mkLabel prettyVarName
                    (fromMaybe (reportError prettyVarName "Numbering")
                          $ lookupSettings varName kv)
        , prefixSubcaptions = getMetaBool "subcaptions" kv
        , prefixSubcaptionsGrid = getMetaBoolDefault "subcaptionsGrid" kv True
        , prefixCaptionPosition = case getMetaString "captionPosition" kv of
            "above" -> Above
            _ -> Below
        , prefixSub = m2p k . (`merge` MetaMap kv') <$> lookupSettings "sub" (Settings (Meta kv') <> from)
        , prefixDef = (`lookupSettings` kv)
        }
        where kv = Settings (Meta kv')
                   <> from
                   <> dtv
                   <> Settings (Meta fmtm)
              fmtm | Just (Format fmt') <- fmt = M.singleton "crossrefOutputFormat" $ MetaString fmt'
                   | otherwise = M.empty
              from | Just fromRef <- M.lookup "from" kv'
                   , Just (MetaMap kv'') <- M.lookup (toString "from" fromRef) m
                   = Settings (Meta kv'')
                   | otherwise = mempty
              getTemplInline = getTemplDefault getMetaInlines
              getTemplBlock = getTemplDefault getMetaBlock
              getTemplDefault f n =
                if isJust $ lookupSettings n kv
                then f n kv
                else reportError n "Template"
              reportError n what = error . T.unpack $ what <> " meta variable " <> n <> " not set for "
                                <> varN <> "." <> k <> ". This should not happen. Please report a bug"
      m2p k _ = error . T.unpack $ "Invalid value for prefix " <> k
    in M.mapWithKey m2p m
  | otherwise = error "Prefixes not defined"
