module Text.Pandoc.CrossRef.Util.Settings (getSettings, defaultMeta) where

import Text.Pandoc hiding (readMarkdown)
import Text.Pandoc.CrossRef.Util.Gap
import Text.Pandoc.Builder
import Control.Exception (handle,IOException)

import Text.Pandoc.CrossRef.Util.Settings.Gen
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.PandocOrphans()

getSettings :: Meta -> IO Meta
getSettings meta = do
  let handler :: IOException -> IO String
      handler _ = return []
  yaml <- handle handler $ readFile (getMetaString "crossrefYaml" (meta <> defaultMeta))
  let Pandoc dtve _ = readMarkdown def ("---\n" ++ yaml ++ "\n---")
  return $ meta <> dtve <> defaultMeta

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
  where var = displayMath
