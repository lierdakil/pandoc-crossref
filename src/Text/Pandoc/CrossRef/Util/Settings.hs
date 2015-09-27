{-# LANGUAGE CPP #-}
module Text.Pandoc.CrossRef.Util.Settings (getSettings, defaultMeta) where

import Text.Pandoc
import Control.Exception (handle,IOException)
import qualified Data.Map as M
import Data.Monoid

import Text.Pandoc.CrossRef.Util.Meta (getMetaString)

getSettings :: Meta -> IO Meta
getSettings meta = do
  let handler :: IOException -> IO String
      handler _ = return []
  yaml <- handle handler $ readFile (getMetaString "crossrefYaml" (meta <> defaultMeta))
#if MIN_VERSION_pandoc(1,14,0)
  let Pandoc dtve _ = either (error . show) id $ readMarkdown def ("---\n" ++ yaml ++ "\n---")
#else
  let Pandoc dtve _ = readMarkdown def ("---\n" ++ yaml ++ "\n---")
#endif
  return $ meta <> dtve <> defaultMeta

defaultMeta :: Meta
defaultMeta = Meta $ M.fromList
  [ ("figureTitle"    , MetaInlines [Str "Figure"])
  , ("tableTitle"     , MetaInlines [Str "Table"])
  , ("listingTitle"   , MetaInlines [Str "Listing"])
  , ("titleDelim"     , MetaInlines [Str ":"])
  , ("chapDelim"      , MetaInlines [Str "."])
  , ("rangeDelim"     , MetaInlines [Str "-"])
  , ("figPrefix"      , MetaList [MetaInlines [Str "fig."], MetaInlines [Str "figs."]])
  , ("eqnPrefix"      , MetaList [MetaInlines [Str "eq."], MetaInlines [Str "eqns."]])
  , ("tblPrefix"      , MetaList [MetaInlines [Str "tbl."], MetaInlines [Str "tbls."]])
  , ("lstPrefix"      , MetaList [MetaInlines [Str "lst."], MetaInlines [Str "lsts."]])
  , ("secPrefix"      , MetaList [MetaInlines [Str "sec."], MetaInlines [Str "secs."]])
  , ("lofTitle"       , MetaBlocks [Header 1 nullAttr [Str "List",Space,Str "of",Space,Str "Figures"]])
  , ("lotTitle"       , MetaBlocks [Header 1 nullAttr [Str "List",Space,Str "of",Space,Str "Tables"]])
  , ("lolTitle"       , MetaBlocks [Header 1 nullAttr [Str "List",Space,Str "of",Space,Str "Listings"]])
  , ("figureTemplate" , MetaInlines [var "figureTitle",Space,var "i",var "titleDelim",Space,var "t"])
  , ("tableTemplate"  , MetaInlines [var "tableTitle",Space,var "i",var "titleDelim",Space,var "t"])
  , ("listingTemplate", MetaInlines [var "listingTitle",Space,var "i",var "titleDelim",Space,var "t"])
  , ("crossrefYaml"   , MetaString "pandoc-crossref.yaml")
  , ("chaptersDepth"  , MetaString "1")
  ]
  where var = Math DisplayMath
