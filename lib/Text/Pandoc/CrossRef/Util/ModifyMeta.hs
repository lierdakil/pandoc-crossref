{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

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
import Control.Monad.Writer

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
    incList = MetaBlocks $ return $ RawBlock (Format "latex") $ unlines $ execWriter $ do
        tell [ "\\makeatletter" ]
        tell subfig
        tell floatnames
        tell listnames
        unless (listings opts) $
          tell codelisting
        tell lolcommand
        when (cref opts) $ do
          tell cleveref
          unless (listings opts) $
            tell cleverefCodelisting
        tell [ "\\makeatother" ]
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
        cleveref = [ usepackage cleverefOpts "cleveref" ]
          <> crefname "figure" figPrefix
          <> crefname "table" tblPrefix
          <> crefname "equation" eqnPrefix
          <> crefname "listing" lstPrefix
          <> crefname "section" secPrefix
        cleverefCodelisting = [
            "\\crefname{codelisting}{\\cref@listing@name}{\\cref@listing@name@plural}"
          , "\\Crefname{codelisting}{\\Cref@listing@name}{\\Cref@listing@name@plural}"
          ]
        cleverefOpts | nameInLink opts = [ "nameinlink" ]
                     | otherwise = []
        crefname n f = [
            "\\crefname{" ++ n ++ "}" ++ prefix f False
          , "\\Crefname{" ++ n ++ "}" ++ prefix f True
          ]
        usepackage [] p = "\\@ifpackageloaded{"++p++"}{}{\\usepackage{"++p++"}}"
        usepackage xs p = "\\@ifpackageloaded{"++p++"}{}{\\usepackage"++o++"{"++p++"}}"
          where o = "[" ++ intercalate "," xs ++ "]"
        toLatex = either (error . show) T.unpack . runPure . writeLaTeX def . Pandoc nullMeta . return . Plain
        metaString s = toLatex $ getMetaInlines s meta
        metaString' s = toLatex [Str $ getMetaString s meta]
        prefix f uc = "{" ++ toLatex (f opts uc 0) ++ "}" ++
                      "{" ++ toLatex (f opts uc 1) ++ "}"
