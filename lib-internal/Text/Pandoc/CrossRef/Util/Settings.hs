{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>
Copyright (C) 2017  Masamichi Hosoda <trueroad@trueroad.jp>

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

{-# LANGUAGE OverloadedStrings, TemplateHaskell, BangPatterns #-}
module Text.Pandoc.CrossRef.Util.Settings (readSettings, defaultMeta, Settings(..)) where

import Text.Pandoc
import Control.Exception (handle,IOException)

import Text.Pandoc.CrossRef.Util.Settings.Embed
import Text.Pandoc.CrossRef.Util.Settings.Util
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Meta
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Map as M

readSettings :: Maybe Format -> Meta -> IO Settings
readSettings fmt inMeta = do
  let meta =
        case inMeta of
          Meta m | Just (MetaMap cm) <- M.lookup "crossref" m
                -> Settings (Meta cm)
          _ -> Settings inMeta
  dirConfig <- readConfig' . T.unpack $ getMetaString "crossrefYaml" (meta <> defaultMeta meta)
  home <- getHomeDirectory
  globalConfig <- readConfig' (home </> ".pandoc-crossref" </> "config.yaml")
  formatConfig <- maybe (return mempty) (readFmtConfig home) fmt
  return $ globalConfig <> formatConfig <> dirConfig <> meta
  where
    readConfig' = handle handler . readConfig
    readFmtConfig home fmt' = readConfig' (home </> ".pandoc-crossref" </> "config-" ++ T.unpack (fmtStr fmt') ++ ".yaml")
    handler :: IOException -> IO Settings
    handler _ = return mempty
    fmtStr (Format fmtstr) = fmtstr

defaultMeta :: Settings -> Settings
defaultMeta userSettings
  | null option = basicMeta
  | option == ["none"] = mempty
  | otherwise = mconcat . reverse $ basicMeta : map name2set option
  where
    option = getMetaStringList "defaultOption" userSettings
    name2set "chapters" = $(embedFile "chapters")
    name2set "subfigures" = $(embedFile "subfigures")
    name2set "numberSections" = $(embedFile "numberSections")
    name2set "titleSections" = $(embedFile "titleSections")
    name2set x = error . T.unpack $ "Unknown defaultOption value: " <> x
    basicMeta = $(embedFile "default")
