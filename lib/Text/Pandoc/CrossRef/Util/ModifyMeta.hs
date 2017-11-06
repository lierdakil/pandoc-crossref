module Text.Pandoc.CrossRef.Util.ModifyMeta
    (
    modifyMeta
    ) where

import Data.List (intercalate)
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.Util
import qualified Data.Text as T

modifyMeta :: Options -> Meta -> Meta
modifyMeta opts meta
  | isFormat "latex" (outFormat opts)
  = setMeta "header-includes"
      (headerInc $ lookupMeta "header-includes" meta)
      meta
  | otherwise = meta
  where
    headerInc :: Maybe MetaValue -> MetaValue
    headerInc Nothing = incList
    headerInc (Just (MetaList x)) = MetaList $ x ++ [incList]
    headerInc (Just x) = MetaList [x, incList]
    incList = MetaBlocks $ return $ RawBlock (Format "latex") $ unlines $
        [ "\\makeatletter" ] ++
        subfig ++
        floatnames ++
        listnames  ++
        [ x | x <- codelisting, not $ listings opts] ++
        lolcommand ++
        [ x | x <- cleveref, cref opts] ++
        [ x | x <- cleverefCodelisting, cref opts && not (listings opts)] ++
        [ "\\makeatother" ]
      where
        subfig = [
            usepackage [] "subfig"
          , usepackage [] "caption"
          , "\\captionsetup[subfloat]{margin=0.5em}"
          ]
        floatnames = [
            "\\AtBeginDocument{%"
          , "\\renewcommand*\\figurename{"++metaString "figureTitle"++"}"
          , "\\renewcommand*\\tablename{"++metaString "tableTitle"++"}"
          , "}"
          ]
        listnames = [
            "\\AtBeginDocument{%"
          , "\\renewcommand*\\listfigurename{"++metaString' "lofTitle"++"}"
          , "\\renewcommand*\\listtablename{"++metaString' "lotTitle"++"}"
          , "}"
          ]
        codelisting = [
            usepackage [] "float"
          , "\\floatstyle{ruled}"
          , "\\@ifundefined{c@chapter}{\\newfloat{codelisting}{h}{lop}}{\\newfloat{codelisting}{h}{lop}[chapter]}"
          , "\\floatname{codelisting}{"++metaString "listingTitle"++"}"
          ]
        lolcommand
          | listings opts = [
              "\\newcommand*\\listoflistings\\lstlistoflistings"
            , "\\AtBeginDocument{%"
            , "\\renewcommand*{\\lstlistlistingname}{"++metaString' "lolTitle"++"}"
            , "}"
            ]
          | otherwise = ["\\newcommand*\\listoflistings{\\listof{codelisting}{"++metaString' "lolTitle"++"}}"]
        cleveref = [
            usepackage cleverefOpts "cleveref"
          , "\\crefname{figure}" ++ prefix figPrefix False
          , "\\crefname{table}" ++ prefix tblPrefix False
          , "\\crefname{equation}" ++ prefix eqnPrefix False
          , "\\crefname{listing}" ++ prefix lstPrefix False
          , "\\crefname{section}" ++ prefix secPrefix False
          , "\\Crefname{figure}" ++ prefix figPrefix True
          , "\\Crefname{table}" ++ prefix tblPrefix True
          , "\\Crefname{equation}" ++ prefix eqnPrefix True
          , "\\Crefname{listing}" ++ prefix lstPrefix True
          , "\\Crefname{section}" ++ prefix secPrefix True
          ]
        cleverefCodelisting = [
            "\\crefname{codelisting}{\\cref@listing@name}{\\cref@listing@name@plural}"
          , "\\Crefname{codelisting}{\\Cref@listing@name}{\\Cref@listing@name@plural}"
          ]
        cleverefOpts | nameInLink opts = "[nameinlink]"
                     | otherwise = ""
        toLatex = either (error . show) T.unpack . runPure . writeLaTeX def . Pandoc nullMeta . return . Plain
        metaString s = toLatex $ getMetaInlines s meta
        metaString' s = toLatex [Str $ getMetaString s meta]
        prefix f uc = "{" ++ toLatex (f opts uc 0) ++ "}" ++
                      "{" ++ toLatex (f opts uc 1) ++ "}"
