module Text.Pandoc.CrossRef.Util.Settings (getSettings, defaultMeta) where

import Text.Pandoc hiding (readMarkdown)
import Text.Pandoc.CrossRef.Util.Gap
import Text.Pandoc.Builder
import Control.Exception (handle,IOException)

import Text.Pandoc.CrossRef.Util.Settings.Gen
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.PandocOrphans()
import System.Directory
import System.FilePath
import System.IO

getSettings :: Maybe Format -> Meta -> IO Meta
getSettings fmt meta = do
  dirConfig <- readConfig (getMetaString "crossrefYaml" (meta <> defaultMeta))
  home <- getHomeDirectory
  globalConfig <- readConfig (home </> ".pandoc-crossref" </> "config.yaml")
  formatConfig <- maybe (return nullMeta) (readFmtConfig home) fmt
  return $ meta <> dirConfig <> formatConfig <> globalConfig <> defaultMeta
  where
    readConfig path =
      handle handler $ do
        h <- openFile path ReadMode
        hSetEncoding h utf8
        yaml <- hGetContents h
        let Pandoc meta' _ = readMarkdown def ("---\n" ++ yaml ++ "\n---")
        return meta'
    readFmtConfig home fmt' = readConfig (home </> ".pandoc-crossref" </> "config-" ++ fmtStr fmt' ++ ".yaml")
    handler :: IOException -> IO Meta
    handler _ = return nullMeta
    fmtStr (Format fmtstr) = fmtstr


defaultMeta :: Meta
defaultMeta =
     cref False
  <> chapters False
  <> chaptersDepth "1"
  <> listings False
  <> codeBlockCaptions False
  <> autoSectionLabels False
  <> numberSections False
  <> sectionsDepth "0"
  <> figLabels "arabic"
  <> eqnLabels "arabic"
  <> tblLabels "arabic"
  <> lstLabels "arabic"
  <> secLabels "arabic"
  <> figureTitle (str "Figure")
  <> tableTitle (str "Table")
  <> listingTitle (str "Listing")
  <> titleDelim (str ":")
  <> chapDelim (str ".")
  <> rangeDelim (str "-")
  <> pairDelim (str "," <> space)
  <> lastDelim (str "," <> space)
  <> refDelim (str "," <> space)
  <> figPrefix [str "fig.", str "figs."]
  <> eqnPrefix [str "eq." , str "eqns."]
  <> tblPrefix [str "tbl.", str "tbls."]
  <> lstPrefix [str "lst.", str "lsts."]
  <> secPrefix [str "sec.", str "secs."]
  <> figPrefixTemplate (var "p" <> str "\160" <> var "i")
  <> eqnPrefixTemplate (var "p" <> str "\160" <> var "i")
  <> tblPrefixTemplate (var "p" <> str "\160" <> var "i")
  <> lstPrefixTemplate (var "p" <> str "\160" <> var "i")
  <> secPrefixTemplate (var "p" <> str "\160" <> var "i")
  <> refIndexTemplate (var "i" <> var "suf")
  <> subfigureRefIndexTemplate (var "i" <> var "suf" <> space <> str "(" <> var "s" <> str ")")
  <> lofTitle (header 1 $ text "List of Figures")
  <> lotTitle (header 1 $ text "List of Tables")
  <> lolTitle (header 1 $ text "List of Listings")
  <> figureTemplate (var "figureTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> tableTemplate (var "tableTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> listingTemplate (var "listingTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> crossrefYaml "pandoc-crossref.yaml"
  <> chaptersDepth "1"
  <> subfigureChildTemplate (var "i")
  <> subfigureTemplate (var "figureTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t" <> str "." <> space <> var "ccs")
  <> subfigLabels "alpha a"
  <> ccsDelim (str "," <> space)
  <> ccsLabelSep (space <> str "—" <> space)
  <> ccsTemplate (var "i" <> var "ccsLabelSep" <> var "t")
  <> tableEqns False
  <> autoEqnLabels False
  <> subfigGrid False
  <> linkReferences False
  <> nameInLink False
  where var = displayMath
