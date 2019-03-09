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
  let meta = Settings inMeta
  dirConfig <- readConfig (getMetaString "crossrefYaml" (meta <> defaultMeta))
  home <- getHomeDirectory
  globalConfig <- readConfig (home </> ".pandoc-crossref" </> "config.yaml")
  formatConfig <- maybe (return mempty) (readFmtConfig home) fmt
  return $ meta <> dirConfig <> formatConfig <> globalConfig
  where
    readConfig path =
      handle handler $ do
        h <- openFile path ReadMode
        hSetEncoding h utf8
        yaml <- hGetContents h
        Pandoc meta' _ <- readMd $ T.pack $ unlines ["---", yaml, "---"]
        return $ Settings meta'
    readMd = handleError . runPure . readMarkdown def{readerExtensions=pandocExtensions}
    readFmtConfig home fmt' = readConfig (home </> ".pandoc-crossref" </> "config-" ++ fmtStr fmt' ++ ".yaml")
    handler :: IOException -> IO Settings
    handler _ = return mempty
    fmtStr (Format fmtstr) = fmtstr


defaultMeta :: Settings
defaultMeta = Settings $
     codeBlockCaptions False
  <> defaultSectionPrefix "sec"
  <> titleDelim (str ":" <> space)
  <> listItemNumberDelim (str "." <> space)
  <> rangeDelim (str "-")
  <> pairDelim (str "," <> space)
  <> lastDelim (str "," <> space)
  <> refDelim (str "," <> space)
  <> crossrefYaml "pandoc-crossref.yaml"
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
  <> numbering "arabic"
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
        "captionPosition" .= "above"
      ],
      "tbl" .: [
        "ref" .= map str ["tbl.", "tbls."],
        "title" .= text "Table"
      ],
      "sec" .: [
        "ref" .= map str ["sec.", "secs."],
        "title" .= text "Section",
        "captionTemplate" .= var "t"
      ]
    ]
  where var = displayMath

prefixes' :: [(String, MetaValue)] -> Meta
prefixes' = prefixes . MetaMap . M.fromList

infixr 0 .:
(.:) :: String -> [(String, MetaValue)] -> (String, MetaValue)
key .: val = (key, MetaMap $ M.fromList val)

infixr 0 .=
(.=) :: ToMetaValue a => String -> a -> (String, MetaValue)
key .= val = (key, toMetaValue val)
