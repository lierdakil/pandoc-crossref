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

{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.CrossRef.Util.Settings (readSettings, defaultMeta, Settings(..)) where

import Text.Pandoc
import Text.Pandoc.Builder hiding ((<>))
import Control.Exception (handle,IOException)

import Text.Pandoc.CrossRef.Util.Settings.Gen
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Meta
import System.Directory
import System.FilePath
import System.IO
import qualified Data.Text as T
import qualified Data.Map as M

readSettings :: Maybe Format -> Meta -> IO Settings
readSettings fmt inMeta = do
  let meta =
        case inMeta of
          Meta m | Just (MetaMap cm) <- M.lookup "crossref" m
                -> Settings (Meta cm)
          _ -> Settings inMeta
  dirConfig <- readConfig . T.unpack $ getMetaString "crossrefYaml" (meta <> defaultMeta meta)
  home <- getHomeDirectory
  globalConfig <- readConfig (home </> ".pandoc-crossref" </> "config.yaml")
  formatConfig <- maybe (return mempty) (readFmtConfig home) fmt
  return $ globalConfig <> formatConfig <> dirConfig <> meta
  where
    readConfig path =
      handle handler $ do
        h <- openFile path ReadMode
        hSetEncoding h utf8
        yaml <- hGetContents h
        Pandoc meta' _ <- readMd $ T.pack $ unlines ["---", yaml, "---"]
        return $ Settings meta'
    readMd = handleError . runPure . readMarkdown def{readerExtensions=pandocExtensions}
    readFmtConfig home fmt' = readConfig (home </> ".pandoc-crossref" </> "config-" ++ T.unpack (fmtStr fmt') ++ ".yaml")
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
    name2set "chapters" = chaptersMeta
    name2set "subfigures" = subfiguresMeta
    name2set "numberSections" = numberSectionsMeta
    name2set x = error . T.unpack $ "Unknown defaultOption value: " <> x

chaptersMeta :: Settings
chaptersMeta = Settings $
     captionIndexTemplate (var "s.i%." <> var "ri")
  <> customMeta [ "scope" .= ("sec" :: T.Text) ]
  <> prefixes' [
      "sec" .: [
        "title" .= ("Chapter" :: T.Text),
        "sub" .: [
          "title" .= ("Section" :: T.Text),
          "referenceIndexTemplate" .= var "i" <> var "suf"
        ]
      ]
  ]

numberSectionsMeta :: Settings
numberSectionsMeta = Settings $
  prefixes' [
    "sec" .: [
      "captionTemplate" .= var "title" <> space <> var "i"
          <> str "." <> space <> var "t"
    ]
  ]

subfiguresMeta :: Settings
subfiguresMeta = Settings $
  prefixes' [
    "fig" .: [
      "subcaptions" .= True,
      "sub" .: [
        "numbering" .= ("alpha a" :: T.Text),
        "referenceIndexTemplate" .= si,
        "listItemTemplate" .= (si <> var "listItemNumberDelim" <> var "t"),
        "captionTemplate" .= var "i",
        "scope" .= ["fig" :: T.Text],
        "captionIndexTemplate" .= var "ri"
      ]
    ]
  ]
  where si = var "s.i" <> str "(" <> var "i" <> str ")"

basicMeta :: Settings
basicMeta = Settings $
     codeBlockCaptions False
  <> adjustSectionIdentifiers False
  <> autoSectionLabels ("sec" :: T.Text)
  <> titleDelim (str ":" <> space)
  <> listItemNumberDelim (str "." <> space)
  <> rangeDelim (str "-")
  <> pairDelim (str "," <> space)
  <> lastDelim (str "," <> space)
  <> refDelim (str "," <> space)
  <> crossrefYaml ("pandoc-crossref.yaml" :: T.Text)
  <> linkReferences False
  <> nameInLink False
  <> collectedCaptionDelim (str "," <> space)
  <> collectedCaptionItemDelim (space <> str "–" <> space)
  -- these are merely the defaults, can (and will) be overridden in prefix configs
  <> captionTemplate (var "title%\160" <> var "i" <> var "titleDelim" <> var "t")
  <> captionIndexTemplate (var "ri")
  <> referenceTemplate (var "Ref[n]%\160" <> var "rs")
  <> listItemTemplate (var "i" <> var "listItemNumberDelim" <> var "t")
  <> collectedCaptionTemplate (var "i" <> var "collectedCaptionItemDelim" <> var "t")
  <> referenceIndexTemplate (var "i" <> var "suf")
  <> listOfTitle (header 1 $ text "List of " <> var "title" <> str "s")
  <> numbering ("arabic" :: T.Text)
  <> prefixes' [
      "eq" .: [
        "ref" .= map str ["eq.", "eqns."],
        "captionTemplate" .= var "t" <> str "\\qquad(" <> var "i" <> str ")",
        "title" .= text "Equation"
      ],
      "fig" .: [
        "ref" .= map str ["fig.", "figs."],
        "title" .= text "Figure"
      ],
      "lst" .: [
        "ref" .= map str ["lst.", "lsts."],
        "title" .= text "Listing",
        "captionPosition" .= ("above" :: T.Text)
      ],
      "tbl" .: [
        "ref" .= map str ["tbl.", "tbls."],
        "title" .= text "Table"
      ],
      "sec" .: [
        "ref" .= map str ["sec.", "secs."],
        "title" .= text "Section",
        "captionTemplate" .= var "t",
        "scope" .= ["sec" :: T.Text],
        "referenceIndexTemplate" .= var "s.refi%." <> var "i" <> var "suf"
      ]
    ]

var :: T.Text -> Inlines
var = displayMath

prefixes' :: [(T.Text, MetaValue)] -> Meta
prefixes' = prefixes . MetaMap . M.fromList

customMeta :: [(T.Text, MetaValue)] -> Meta
customMeta = Meta . M.fromList

infixr 0 .:
(.:) :: T.Text -> [(T.Text, MetaValue)] -> (T.Text, MetaValue)
key .: val = (key, MetaMap $ M.fromList val)

infixr 0 .=
(.=) :: ToMetaValue a => T.Text -> a -> (T.Text, MetaValue)
key .= val = (key, toMetaValue val)
