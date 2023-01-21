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

{-# LANGUAGE Rank2Types, OverloadedStrings, FlexibleContexts #-}

module Text.Pandoc.CrossRef.References.Blocks.Math where

import Control.Monad.Reader.Class
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util (setLabel, replaceAttr)
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

runBlockMath :: Attr -> T.Text -> WS (ReplacedResult Block)
runBlockMath (label, cls, attrs) eq = do
  opts <- ask
  if tableEqns opts && not (isLatexFormat (outFormat opts))
  then do
    (eq', idxStr) <- replaceEqn (label, cls, attrs) eq
    let mathfmt = if eqnBlockInlineMath opts then InlineMath else DisplayMath
    replaceNoRecurse $ Div (label,cls,setLabel opts idxStr attrs) $
      applyTemplate [Math mathfmt $ stringify idxStr] [Math mathfmt eq']
        $ eqnBlockTemplate opts
  else noReplaceRecurse

replaceEqn :: Attr -> T.Text -> WS (T.Text, [Inline])
replaceEqn (label, _, attrs) eq = do
  opts <- ask
  let label' | T.null label = Left "eq"
             | otherwise = Right label
  idxStrRaw <- replaceAttr label' (lookup "label" attrs) [] eqnRefs
  let idxStr = applyTemplate' (M.fromDistinctAscList [("i", idxStrRaw)]) $ eqnIndexTemplate opts
      eqTxt = applyTemplate' eqTxtVars $ eqnInlineTemplate opts :: [Inline]
      eqTxtVars = M.fromDistinctAscList
        [ ("e", [Str eq])
        , ("i", idxStr)
        , ("ri", idxStrRaw)
        ]
      eq' | tableEqns opts = eq
          | otherwise = stringify eqTxt
  return (eq', idxStr)

splitMath :: [Block] -> [Block]
splitMath (Para ils:xs)
  | length ils > 1 = map Para (split [] [] ils) <> xs
  where
    split res acc [] = reverse (reverse acc : res)
    split res acc (x@(Span _ [Math DisplayMath _]):ys) =
      split ([x] : reverse (dropSpaces acc) : res)
            [] (dropSpaces ys)
    split res acc (y:ys) = split res (y:acc) ys
    dropSpaces = dropWhile isSpace
splitMath xs = xs
