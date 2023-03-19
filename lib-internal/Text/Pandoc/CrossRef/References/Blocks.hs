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

{-# LANGUAGE Rank2Types, OverloadedStrings, FlexibleContexts, ScopedTypeVariables, LambdaCase #-}
module Text.Pandoc.CrossRef.References.Blocks
  ( replaceAll
  ) where

import Control.Monad.Reader
import Data.List
import qualified Data.Text as T
import Lens.Micro
import Text.Pandoc.Definition
import Text.Pandoc.Shared (blocksToInlines)

import Text.Pandoc.CrossRef.References.Blocks.CodeBlock
import Text.Pandoc.CrossRef.References.Blocks.Header
import Text.Pandoc.CrossRef.References.Blocks.Math
import Text.Pandoc.CrossRef.References.Blocks.Subfigures
import Text.Pandoc.CrossRef.References.Blocks.Table
import Text.Pandoc.CrossRef.References.Blocks.Util
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util

replaceAll :: (Data a) => a -> WS a
replaceAll x = do
  opts <- ask
  x & runReplace (mkRR replaceBlock
    `extRR` replaceInlineMany
    )
    . runSplitMath opts
    . everywhere (mkT divBlocks `extT` spanInlines opts)
  where
    runSplitMath opts
      | tableEqns opts
      , not $ isLatexFormat (outFormat opts)
      = everywhere (mkT splitMath)
      | otherwise = id

extractCaption :: Block -> Maybe [Inline]
extractCaption = \case
  Para caption -> Just caption
  Div (_, dcls, _) [Para caption] | "caption" `elem` dcls -> Just caption
  _ -> Nothing

replaceBlock :: Block -> WS (ReplacedResult Block)
replaceBlock (Header n attr text') = runHeader n attr text'
replaceBlock (Figure attr@(label, _, _) caption content)
  | "fig:" `T.isPrefixOf` label
  = runFigure False attr caption content
replaceBlock (Div attr@(label, _, _) content)
  | "fig:" `T.isPrefixOf` label
  , Just caption <- extractCaption $ last content
  = case init content of
      [Figure ("", [], []) _ content'] -- nested figure due to implicit_figures...
        -> runFigure False attr (Caption Nothing [Para caption]) content'
      xs -> runSubfigures attr xs caption
replaceBlock (Div attr@(label, _, _) [Table tattr (Caption short (btitle:rest)) colspec header cells foot])
  | not $ null $ blocksToInlines [btitle]
  , "tbl:" `T.isPrefixOf` label
  = runTable attr (Just tattr) short btitle rest colspec header cells foot
replaceBlock (Table attr@(label, _, _) (Caption short (btitle:rest)) colspec header cells foot)
  | not $ null $ blocksToInlines [btitle]
  , "tbl:" `T.isPrefixOf` label
  = runTable attr Nothing short btitle rest colspec header cells foot
replaceBlock (CodeBlock attr@(label, _, attrs) code)
  | not $ T.null label
  , "lst:" `T.isPrefixOf` label
  , Just caption <- lookup "caption" attrs
  = runCodeBlock attr code $ Left caption
replaceBlock
  (Div (label,"listing":divClasses, divAttrs)
    [Para caption, CodeBlock ("",cbClasses,cbAttrs) code])
  | not $ T.null label
  , "lst:" `T.isPrefixOf` label
  = runCodeBlock (label, nub $ divClasses <> cbClasses, divAttrs <> cbAttrs) code $ Right caption
replaceBlock (Para [Span attr [Math DisplayMath eq]])
  = runBlockMath attr eq
replaceBlock _ = noReplaceRecurse

replaceInlineMany :: [Inline] -> WS (ReplacedResult [Inline])
replaceInlineMany (Span spanAttr@(label,clss,attrs) [Math DisplayMath eq]:xs) = do
  opts <- ask
  if "eq:" `T.isPrefixOf` label || T.null label && autoEqnLabels opts
  then do
    replaceRecurse . (<> xs) =<< if isLatexFormat $ outFormat opts
      then
        pure [RawInline (Format "latex") "\\begin{equation}"
        , Span spanAttr [RawInline (Format "latex") eq]
        , RawInline (Format "latex") $ mkLaTeXLabel label <> "\\end{equation}"]
      else do
        (eq', idxStr) <- replaceEqn spanAttr eq
        pure [Span (label,clss,setLabel opts idxStr attrs) [Math DisplayMath eq']]
  else noReplaceRecurse
replaceInlineMany _ = noReplaceRecurse

divBlocks :: Block -> Block
divBlocks (Table tattr (Caption short (btitle:rest)) colspec header cells foot)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = Div (label,[],[]) [
    Table tattr (Caption short $ walkReplaceInlines (dropWhileEnd isSpace (init title)) title btitle:rest) colspec header cells foot]
  where
    title = blocksToInlines [btitle]
divBlocks x = x

spanInlines :: Options -> [Inline] -> [Inline]
spanInlines opts (math@(Math DisplayMath _eq):ils)
  | c:ils' <- dropWhile isSpace ils
  , Just label <- getRefLabel "eq" [c]
  = Span (label,[],[]) [math]:ils'
  | autoEqnLabels opts
  = Span nullAttr [math]:ils
spanInlines _ x = x
