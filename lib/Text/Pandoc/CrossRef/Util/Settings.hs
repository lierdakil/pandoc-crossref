module Text.Pandoc.CrossRef.Util.Settings (getSettings, defaultMeta) where

import Text.Pandoc hiding (readMarkdown)
import Text.Pandoc.CrossRef.Util.Gap
import Text.Pandoc.Builder
import Control.Exception (handle,IOException)

import Text.Pandoc.CrossRef.Util.Settings.Gen
import Text.Pandoc.CrossRef.Util.Meta

getSettings :: Meta -> IO Meta
getSettings meta = do
  let handler :: IOException -> IO String
      handler _ = return []
  yaml <- handle handler $ readFile (getMetaString "crossrefYaml" (meta <> defaultMeta))
  let Pandoc dtve _ = readMarkdown def ("---\n" ++ yaml ++ "\n---")
  return $ meta <> dtve <> defaultMeta

defaultMeta :: Meta
defaultMeta =
     cref (MetaBool False)
  <> chapters (MetaBool False)
  <> chaptersDepth (MetaString "1")
  <> listings (MetaBool False)
  <> codeBlockCaptions (MetaBool False)
  <> autoSectionLabels (MetaBool False)
  <> figLabels (MetaString "arabic")
  <> eqnLabels (MetaString "arabic")
  <> tblLabels (MetaString "arabic")
  <> lstLabels (MetaString "arabic")
  <> secLabels (MetaString "arabic")
  <> figureTitle (str "Figure")
  <> tableTitle (str "Table")
  <> listingTitle (str "Listing")
  <> titleDelim (str ":")
  <> chapDelim (str ".")
  <> rangeDelim (str "-")
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
  <> lofTitle (header 1 $ text "List of Figures")
  <> lotTitle (header 1 $ text "List of Tables")
  <> lolTitle (header 1 $ text "List of Listings")
  <> figureTemplate (var "figureTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> tableTemplate (var "tableTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> listingTemplate (var "listingTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t")
  <> crossrefYaml (MetaString "pandoc-crossref.yaml")
  <> chaptersDepth (MetaString "1")
  <> subfigureChildTemplate (var "i")
  <> subfigureTemplate (var "figureTitle" <> space <> var "i" <> var "titleDelim" <> space <> var "t" <> str "." <> space <> var "ccs")
  <> subfigLabels (MetaString "alpha a")
  <> ccsDelim (str "," <> space)
  <> ccsLabelSep (space <> str "—" <> space)
  <> ccsTemplate (var "i" <> var "ccsLabelSep" <> var "t")
  <> tableEqns (MetaBool False)
  where var = displayMath
