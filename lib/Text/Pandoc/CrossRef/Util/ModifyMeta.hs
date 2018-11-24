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
import Text.Pandoc.Shared (blocksToInlines)
import Text.Pandoc.Builder hiding ((<>))
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Prefixes
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Util
import qualified Data.Text as T
import Control.Monad.Writer

modifyMeta :: Options -> Settings -> Meta
modifyMeta opts meta
  | isLatexFormat (outFormat opts)
  = setMeta "header-includes"
      (headerInc $ lookupSettings "header-includes" meta)
      $ unSettings meta
  | otherwise = unSettings meta
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
          , "\\renewcommand*\\figurename{"++getFloatCaption "fig"++"}"
          , "\\renewcommand*\\tablename{"++getFloatCaption "tbl"++"}"
          , "}"
          ]
        listnames = [
            "\\AtBeginDocument{%"
          , "\\renewcommand*\\listfigurename{"++getListOfTitle "fig"++"}"
          , "\\renewcommand*\\listtablename{"++getListOfTitle "tbl"++"}"
          , "}"
          ]
        codelisting = [
            usepackage [] "float"
          , "\\floatstyle{ruled}"
          , "\\@ifundefined{c@chapter}{\\newfloat{codelisting}{h}{lop}}{\\newfloat{codelisting}{h}{lop}[chapter]}"
          , "\\floatname{codelisting}{"++getListOfTitle "lst"++"}"
          ]
        lolcommand
          | listings opts = [
              "\\newcommand*\\listoflistings\\lstlistoflistings"
            , "\\AtBeginDocument{%"
            , "\\renewcommand*{\\lstlistlistingname}{"++getListOfTitle "lst"++"}"
            , "}"
            ]
          | otherwise = ["\\newcommand*\\listoflistings{\\listof{codelisting}{"++getListOfTitle "lst"++"}}"]
        cleveref = [ usepackage cleverefOpts "cleveref" ]
          <> crefname "figure" (pfxRef "fig")
          <> crefname "table" (pfxRef "tbl")
          <> crefname "equation" (pfxRef "eq")
          <> crefname "listing" (pfxRef "lst")
          <> crefname "section" (pfxRef "sec")
        pfxRef labelPrefix = prefixRef . flip getPfx labelPrefix
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
        getListOfTitle = toLatex . blocksToInlines . toList . getTitleForListOf opts
        getFloatCaption = toLatex . toList . prefixTitle . getPfx opts
        prefix f uc = "{" ++ toLatex (toList $ f opts uc 0) ++ "}" ++
                      "{" ++ toLatex (toList $ f opts uc 1) ++ "}"
