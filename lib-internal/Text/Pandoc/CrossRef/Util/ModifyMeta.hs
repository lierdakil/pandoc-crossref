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

{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Text.Pandoc.CrossRef.Util.ModifyMeta
    (
    modifyMeta
    ) where

import Text.Pandoc
import Text.Pandoc.Builder hiding ((<>))
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Util
import qualified Data.Text as T

modifyMeta :: Meta -> CrossRef Meta
modifyMeta meta = do
  Options{..} <- asks creOptions
  Settings (Meta settingskv) <- asks creSettings
  let
    headerInc :: Maybe MetaValue -> MetaValue
    headerInc Nothing = incList
    headerInc (Just (MetaList x)) = MetaList $ x <> [incList]
    headerInc (Just x) = MetaList [x, incList]
    incList = MetaBlocks $ return $ RawBlock (Format "latex") $ T.unlines $ execWriter $ do
        tell [ "\\makeatletter" ]
        tell [ "\\@ifpackageloaded{caption}{\\captionsetup{labelformat=empty}}{\\usepackage[labelformat=empty]{caption}}" ]
        tell [ "\\newenvironment{pandoccrossrefsubcaption}{\\renewcommand{\\toprule}{}\\renewcommand{\\bottomrule}{}}{}" ]
        tell [ "\\makeatother" ]
    tweakedMeta
      | Just _ <- lookupMeta "crossref" meta = setMeta "crossref" (MetaMap settingskv) meta
      | otherwise = Meta settingskv
  return $ if isLatexFormat outFormat
  then setMeta "header-includes"
      (headerInc $ lookupMeta "header-includes" meta)
      tweakedMeta
  else tweakedMeta
