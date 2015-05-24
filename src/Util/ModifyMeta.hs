module Util.ModifyMeta
    (
    modifyMeta
    ) where

import Text.Pandoc.Builder
import Data.Maybe (fromMaybe)
import Util.Options
import Util.Meta

modifyMeta :: Options -> Meta -> Meta
modifyMeta opts meta
  = setMeta "header-includes"
      (headerInc $ lookupMeta "header-includes" meta)
      meta
  where
    headerInc :: Maybe MetaValue -> MetaValue
    headerInc Nothing = MetaList incList
    headerInc (Just x@(MetaString _)) = MetaList $ x:incList
    headerInc (Just (MetaList x)) = MetaList $ x ++ incList
    headerInc (Just x) = x
    incList = map MetaString $
        [ x | x <- floatnames] ++
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
          ]
        cleverefCodelisting = [
            "\\makeatletter"
          , "\\crefname{codelisting}{\\cref@listing@name}{\\cref@listing@name@plural}"
          , "\\makeatother"
          ]
        metaString s = fromMaybe "" $ lookupMeta s meta >>= toString
