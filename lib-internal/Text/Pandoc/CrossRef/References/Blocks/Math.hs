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

module Text.Pandoc.CrossRef.References.Blocks.Math where

import Control.Monad.Reader.Class
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)

import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

runBlockMath :: Attr -> T.Text -> WS (ReplacedResult Block)
runBlockMath (label, cls, attrs) eq = do
  opts <- ask
  if tableEqns opts && not (isLatexFormat opts)
  then do
    ReplaceEqn{..} <- replaceEqn eqnBlockTemplate (label, cls, attrs) eq
    replaceNoRecurse $ Div (label,cls,setLabel opts replaceEqnIdx attrs) replaceEqnEq
  else noReplaceRecurse

data ReplaceEqn a = ReplaceEqn
  { replaceEqnEq :: [a]
  , replaceEqnIdx :: [Inline]
  }

replaceEqn :: MkTemplate a t => (Options -> t) -> Attr -> T.Text -> WS (ReplaceEqn a)
replaceEqn eqTemplate (label, _, attrs) eq = do
  opts <- ask
  let label' | T.null label = Left "eq"
             | otherwise = Right label
  idxStrRaw <- replaceAttr label' attrs [] SPfxEqn
  let idxStr = applyTemplate' (M.fromDistinctAscList [("i", idxStrRaw)]) $ eqnIndexTemplate opts
      eqTxt :: [Inline]
      eqTxt = applyTemplate' eqTxtVars $
        if tableEqns opts
        then eqnInlineTableTemplate opts
        else eqnInlineTemplate opts
      wrapMath x = [Math mathfmt $ stringify x]
      commonVars eqn = M.fromList $
        [ ("e", eqn)
        , ("t", eqn) -- backwards compatibility for eqnBlockTemplate
        , ("i", wrapMath idxStr)
        , ("ri", wrapMath idxStrRaw)
        , ("nmi", idxStr)
        , ("nmri", idxStrRaw)
        ]
      eqTxtVars = commonVars [Str eq]
      eqInline = applyTemplate' eqInlineVars $ eqTemplate opts
      mathfmt = if eqnBlockInlineMath opts then InlineMath else DisplayMath
      eqInlineVars = commonVars $ wrapMath eqTxt
  pure $ ReplaceEqn
    { replaceEqnEq = eqInline
    , replaceEqnIdx = idxStr
    }


splitMath :: [Block] -> [Block]
splitMath (Para ils:xs)
  | _:_:_ <- ils -- at least two elements
  = map Para (split ils) <> xs
  where
    split ys =
      let (before, after) = break isMath ys
          beforeEl
            | null before = id
            | otherwise = (before :)
      in beforeEl $ case after of
        z:zs -> [z] : split (dropSpaces zs)
        [] -> []
    dropSpaces = dropWhile isSpace
    isMath (Span _ [Math DisplayMath _]) = True
    isMath _ = False
splitMath xs = xs
