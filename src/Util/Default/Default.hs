module Util.Default.Default where

import Text.Pandoc.Definition

getDefaultMeta :: String -> Maybe MetaValue
getDefaultMeta varname = case varname of
  "figureTitle"    -> Just $ MetaInlines [Str "Figure"]
  "tableTitle"     -> Just $ MetaInlines [Str "Table"]
  "titleDelim"     -> Just $ MetaInlines [Str ":"]
  "chapDelim"      -> Just $ MetaInlines [Str "."]
  "rangeDelim"     -> Just $ MetaInlines [Str "-"]
  "figPrefix"      -> Just $ MetaInlines [Str "fig."]
  "eqnPrefix"      -> Just $ MetaInlines [Str "eq."]
  "tblPrefix"      -> Just $ MetaInlines [Str "tbl."]
  "lofTitle"       -> Just $ MetaBlocks [Header 1 nullAttr [Str "List of Figures"]]
  "lotTitle"       -> Just $ MetaBlocks [Header 1 nullAttr [Str "List of Tables"]]
  "figureTemplate" -> Just $ MetaInlines [var "figureTitle",Space,var "i",var "titleDelim",Space,var "t"]
  "tableTemplate"  -> Just $ MetaInlines [var "tableTitle",Space,var "i",var "titleDelim",Space,var "t"]
  "crossrefYaml"   -> Just $ MetaString "pandoc-crossref.yaml"
  _                -> Nothing
  where var = Math DisplayMath
