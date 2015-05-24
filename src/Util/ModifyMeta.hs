module Util.ModifyMeta
    (
    modifyMeta
    ) where

import Text.Pandoc
import Text.Pandoc.Builder
import Util.Options
import Util.Meta
import Util.Util

modifyMeta :: Options -> Meta -> Meta
modifyMeta opts meta
  | isFormat "latex" (outFormat opts)
  = setMeta "header-includes"
      (headerInc $ lookupMeta "header-includes" meta)
      meta
  | otherwise = meta
  where
    headerInc :: Maybe MetaValue -> MetaValue
    headerInc Nothing = MetaList incList
    headerInc (Just x@(MetaString _)) = MetaList $ x:incList
    headerInc (Just (MetaList x)) = MetaList $ x ++ incList
    headerInc (Just x) = x
    incList = map MetaString $
        floatnames ++
        [ x | x <- codelisting, not $ useListings opts] ++
        [ x | x <- cleveref, useCleveref opts] ++
        [ x | x <- cleverefCodelisting, useCleveref opts && not (useListings opts)] ++
        []
      where
        floatnames = [
            "\\usepackage{float}"
          , "\\floatname{figure}{"++metaString "figureTitle"++"}"
          , "\\floatname{longtable}{"++metaString "tableTitle"++"}"
          ]
        codelisting = [
            "\\floatstyle{ruled}"
          , "\\newfloat{codelisting}{h}{lop}"
          , "\\floatname{codelisting}{"++metaString "listingTitle"++"}"
          ]
        cleveref = [
            "\\usepackage{cleveref}"
          , "\\crefname{figure}" ++ prefix "figPrefix"
          , "\\crefname{table}" ++ prefix "tblPrefix"
          , "\\crefname{equation}" ++ prefix "eqnPrefix"
          , "\\crefname{listing}" ++ prefix "lstPrefix"
          ]
        cleverefCodelisting = [
            "\\makeatletter"
          , "\\crefname{codelisting}{\\cref@listing@name}{\\cref@listing@name@plural}"
          , "\\makeatother"
          ]
        toLatex = writeLaTeX def . Pandoc nullMeta . return . Plain
        metaString s = toLatex $ getMetaInlines s meta
        metaList s = toLatex . getMetaList toInlines s meta
        prefix s = "{" ++ metaList s 0 ++ "}" ++
                   "{" ++ metaList s 1 ++ "}"
