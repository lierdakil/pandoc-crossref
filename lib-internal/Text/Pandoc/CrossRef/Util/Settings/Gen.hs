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

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Text.Pandoc.CrossRef.Util.Settings.Gen where

import Text.Pandoc.CrossRef.Util.Settings.Template
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Options as O (Options(..))
import Language.Haskell.TH (mkName)
import Text.Pandoc.Definition

nameDeriveSetters ''Options

concat <$> mapM (makeAcc . mkName)
  [ "crossrefYaml"
  , "titleDelim"
  , "listItemNumberDelim"
  , "captionTemplate"
  , "referenceTemplate"
  , "referenceIndexTemplate"
  , "captionIndexTemplate"
  , "listItemTemplate"
  , "numbering"
  , "listOfTitle"
  , "collectedCaptionDelim"
  , "collectedCaptionItemDelim"
  , "collectedCaptionTemplate"
  ]

getOptions :: Settings -> Maybe Format -> Options
getOptions dtv fmt = $(makeCon ''Options 'Options)
