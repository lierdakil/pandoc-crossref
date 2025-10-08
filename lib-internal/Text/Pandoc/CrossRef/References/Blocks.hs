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

module Text.Pandoc.CrossRef.References.Blocks
  ( replaceAll
  ) where

import Lens.Micro.Mtl
import Data.List
import qualified Data.Text as T
import Lens.Micro
import Text.Pandoc.Definition
import Text.Pandoc.Shared (blocksToInlines)

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.List
import Text.Pandoc.CrossRef.References.Blocks.CodeBlock
import Text.Pandoc.CrossRef.References.Blocks.Header
import Text.Pandoc.CrossRef.References.Blocks.Math
import Text.Pandoc.CrossRef.References.Blocks.Subfigures
import Text.Pandoc.CrossRef.References.Blocks.Table
import Text.Pandoc.CrossRef.References.Blocks.Util
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.CodeBlockCaptions
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Generic
import Text.Pandoc.CrossRef.Util.Util

replaceAll :: (Data a) => a -> WS a
replaceAll x = do
  opts <- use wsOptions
  x & everywhere (mkT (spanInlines opts) `extT` doSplitMath opts) -- bottom-up pass
    & runReplace (mkRR replaceBlock -- top-down pass
      `extRR` replaceInlineMany
      `extRR` replaceBlockMany
      )
  where
    doSplitMath opts
      | tableEqns opts
      , not $ isLatexFormat opts
      = splitMath
      | otherwise = id

extractCaption :: Block -> Maybe [Inline]
extractCaption = \case
  Para caption -> Just caption
  Div (_, dcls, _) [Para caption] | "caption" `elem` dcls -> Just caption
  _ -> Nothing

replaceBlock :: Block -> WS (ReplacedResult Block)
replaceBlock (Header n attr text') = runHeader n attr text'
replaceBlock (Figure attr@(label, _, _) caption content)
  | label `hasPfx` PfxImg
  = runFigure False attr caption content
replaceBlock (Div attr@(label, _, _) content)
  | label `hasPfx` PfxImg
  , Just caption <- extractCaption $ last content
  = case init content of
      [Figure ("", [], []) _ content'] -- nested figure due to implicit_figures...
        -> runFigure False attr (Caption Nothing [Para caption]) content'
      xs -> runSubfigures attr xs caption
replaceBlock (Div attr@(label, _, _) [Table tattr (Caption short (btitle:rest)) colspec header cells foot])
  | not $ null $ blocksToInlines [btitle]
  , label `hasPfx` PfxTbl
  = runTable attr (Just tattr) short btitle rest colspec header cells foot
replaceBlock (Table attr@(label, _, _) (Caption short (btitle:rest)) colspec header cells foot)
  | not $ null $ blocksToInlines [btitle]
  , label `hasPfx` PfxTbl
  = runTable attr Nothing short btitle rest colspec header cells foot
replaceBlock (CodeBlock attr@(label, _, attrs) code)
  | not $ T.null label
  , label `hasPfx` PfxLst
  , Just caption <- lookup "caption" attrs
  = runCodeBlock attr code $ Left caption
replaceBlock
  (Div (label,"listing":divClasses, divAttrs)
    [Para caption, CodeBlock ("",cbClasses,cbAttrs) code])
  | not $ T.null label
  , label `hasPfx` PfxLst
  = runCodeBlock (label, nub $ divClasses <> cbClasses, divAttrs <> cbAttrs) code $ Right caption
replaceBlock (Para [Span attr [Math DisplayMath eq]])
  = runBlockMath attr eq
replaceBlock _ = noReplaceRecurse

replaceInlineMany :: [Inline] -> WS (ReplacedResult [Inline])
replaceInlineMany (Span spanAttr@(label,clss,attrs) [Math DisplayMath eq]:xs) = do
  opts <- use wsOptions
  if label `hasPfx` PfxEqn || T.null label && autoEqnLabels opts
  then do
    res <- if isLatexFormat opts
      then
        pure [RawInline (Format "latex") "\\begin{equation}"
        , Span spanAttr [RawInline (Format "latex") eq]
        , RawInline (Format "latex") "\\end{equation}"]
      else do
        ReplaceEqn{replaceEqnEq, replaceEqnIdx} <- replaceEqn eqnDisplayTemplate spanAttr eq
        pure [Span (label,clss,setLabel opts replaceEqnIdx attrs) replaceEqnEq]
    replaceList res xs
  else noReplaceRecurse
replaceInlineMany (x:xs) = fixRefs' x xs
replaceInlineMany [] = noReplaceRecurse

replaceBlockMany :: [Block] -> WS (ReplacedResult [Block])
replaceBlockMany bs@(x:xs) = do
  opts <- use wsOptions
  case mkCodeBlockCaptions opts bs of
    Just res' -> replaceRecurse res'
    Nothing -> liftF (listOf x opts) `fixRefs` xs
replaceBlockMany [] = noReplaceRecurse

spanInlines :: Options -> [Inline] -> [Inline]
spanInlines opts (math@(Math DisplayMath _eq):ils)
  | c:ils' <- dropWhile isSpace ils
  , Just label <- getRefLabel PfxEqn [c]
  = Span (label,[],[]) [math]:ils'
  | autoEqnLabels opts
  = Span nullAttr [math]:ils
spanInlines _ x = x
