module Util.Default.Default where

import Text.Pandoc.Definition
import qualified Data.Map as M

defaultMeta :: Meta
defaultMeta = Meta $ M.fromList
  [ ("figureTitle"    , MetaInlines [Str "Figure"])
  , ("tableTitle"     , MetaInlines [Str "Table"])
  , ("titleDelim"     , MetaInlines [Str ":"])
  , ("chapDelim"      , MetaInlines [Str "."])
  , ("rangeDelim"     , MetaInlines [Str "-"])
  , ("figPrefix"      , MetaInlines [Str "fig."])
  , ("eqnPrefix"      , MetaInlines [Str "eq."])
  , ("tblPrefix"      , MetaInlines [Str "tbl."])
  , ("lofTitle"       , MetaBlocks [Header 1 nullAttr [Str "List of Figures"]])
  , ("lotTitle"       , MetaBlocks [Header 1 nullAttr [Str "List of Tables"]])
  , ("figureTemplate" , MetaInlines [var "figureTitle",Space,var "i",var "titleDelim",Space,var "t"])
  , ("tableTemplate"  , MetaInlines [var "tableTitle",Space,var "i",var "titleDelim",Space,var "t"])
  , ("crossrefYaml"   , MetaString "pandoc-crossref.yaml")
  ]
  where var = Math DisplayMath
