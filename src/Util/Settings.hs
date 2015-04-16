module Util.Settings (getSettings, defaultMeta) where

import Text.Pandoc
import Control.Exception (handle,IOException)
import qualified Data.Map as M
import Data.Monoid

import Util.Meta (getMetaString)

getSettings :: Meta -> IO Meta
getSettings meta = do
  let handler :: IOException -> IO String
      handler _ = return []
  yaml <- handle handler $ readFile (getMetaString "crossrefYaml" (meta <> defaultMeta))
  let Pandoc dtve _ = readMarkdown def ("---\n" ++ yaml ++ "\n---")
  return $ meta <> dtve <> defaultMeta

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
