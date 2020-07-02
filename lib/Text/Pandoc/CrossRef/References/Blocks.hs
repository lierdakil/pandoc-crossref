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

{-# LANGUAGE RecordWildCards, NamedFieldPuns, TupleSections, OverloadedStrings #-}
module Text.Pandoc.CrossRef.References.Blocks
  ( replaceAll
  ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Shared (stringify, makeSections, blocksToInlines)
import Text.Pandoc.Walk (walk)
import Control.Monad.State hiding (get, modify)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Pandoc.CrossRef.References.Types as Types
import Text.Pandoc.CrossRef.References.Subfigures
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Prefixes
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.CustomLabels
import Text.Pandoc.CrossRef.Util.CodeBlockCaptions
import Text.Pandoc.CrossRef.Util.VarFunction
import Text.Pandoc.CrossRef.Util.Replace
import Control.Applicative
import Control.Arrow (second)
import Data.Default (def)
import Prelude

replaceAll :: [Block] -> WS [Block]
replaceAll bs =
  asks creOptions >>= \opts ->
  let run =
          fmap unhierarchicalize
        . runReplace [] (mkRR (replaceBlock opts) `extRR` replaceInline opts)
        . makeSections False Nothing
        . everywhere (mkT $ makeSubfigures opts)
        . everywhere (mkT (divBlocks opts) `extT` spanInlines opts)
        . everywhere (mkT $ mkCodeBlockCaptions opts)
  in run bs

replaceBlock :: Options -> Scope -> Block -> WS (ReplacedResult Block)
-- sections
replaceBlock opts scope (Div (ident, "section":cls, attr) (Header lvl (hident, hcls, hattr) text' : body))
  | Just (pfx, label') <-
          fmap (, ExplicitLabel label) (getRefPrefix opts label)
      <|> (autoSectionLabels opts >>= \asl -> return $
              if adjustSectionIdentifiers opts
              then let l = asl <> ":" <> label in (asl, ExplicitLabel l)
              else (asl, AutoLabel)
          )
  = let newlabel = fromMaybe label $ labelToMaybe label'
        (newident, newhident)
          | T.null hident = (newlabel, hident)
          | otherwise = (ident, newlabel)
        result title = Div (newident, "section":cls, attr) (Header lvl (newhident, hcls, hattr) title : body)
    in if "unnumbered" `elem` hcls
    then replaceRecurse scope $ result text'
    else do
      let ititle = B.fromList text'
      rec' <- replaceAttr opts scope label' hattr ititle pfx
      let title' = B.toList $ refCaption rec'
      replaceRecurse (newScope rec' scope) $ result title'
  where label | T.null hident = ident
              | otherwise = hident
-- tables
replaceBlock opts scope (Div divOps@(label,_,attrs) [Table tattr (Caption short (btitle:rest)) colspec header cells foot])
  | not $ null title
  , Just pfx <- getRefPrefix opts label
  = do
    let ititle = B.fromList title
    RefRec{..} <- replaceAttr opts scope (ExplicitLabel label) attrs ititle pfx
    let caption' = Caption short (walkReplaceInlines (B.toList refCaption) title btitle:rest)
    replaceNoRecurse $ Div divOps [Table tattr caption' colspec header cells foot]
  where title = blocksToInlines [btitle]
replaceBlock opts scope (Table divOps@(label,_,attrs) (Caption short (btitle:rest)) colspec header cells foot)
  | not $ null title
  , Just pfx <- getRefPrefix opts label
  = do
    let ititle = B.fromList title
    RefRec{..} <- replaceAttr opts scope (ExplicitLabel label) attrs ititle pfx
    let caption' = Caption short (walkReplaceInlines (B.toList refCaption) title btitle:rest)
    replaceNoRecurse $ Table divOps caption' colspec header cells foot
  where title = blocksToInlines [btitle]
-- code blocks
replaceBlock opts scope (Div (label, [], divattrs) [CodeBlock ("",classes,cbattrs) code, Para (Str ":":Space:caption)])
  | Just pfx <- getRefPrefix opts label
  = do
    ref <- replaceAttr opts scope (ExplicitLabel label) divattrs (B.fromList caption) pfx
    replaceNoRecurse $
      Div (label, "listing":classes, [])
        $ placeCaption opts ref [CodeBlock ("", classes, cbattrs) code]
-- Generic div
replaceBlock opts scope (Div ats@(label, _, attrs) content)
  | Just pfx <- getRefPrefix opts label
  = do
    let (caption, content')
          | not (null content)
          , Para (Str ":":Space:c) <- last content
          = (B.fromList c, init content)
          | otherwise = (mempty, content)
    ref <- replaceAttr opts scope (ExplicitLabel label) attrs caption pfx
    replaceRecurse (newScope ref scope) $
      Div ats $ placeCaption opts ref content'
replaceBlock _ scope _ = noReplaceRecurse scope

placeCaption :: Options -> RefRec -> [Block] -> [Block]
placeCaption opts RefRec{..} body
  | Above <- refCaptionPosition
  = mkCaption opts "Caption" refCaption : body
  | Below <- refCaptionPosition
  = body <> [mkCaption opts "Caption" refCaption]

data ItemLabel = ExplicitLabel T.Text | AutoLabel

labelToMaybe :: ItemLabel -> Maybe T.Text
labelToMaybe (ExplicitLabel s) = Just s
labelToMaybe AutoLabel = Nothing

autoLabel :: T.Text -> T.Text -> Maybe ItemLabel
autoLabel pfx label
  | T.null label = Just AutoLabel
  | (pfx <> ":") `T.isPrefixOf` label = Just $ ExplicitLabel label
  | otherwise = Nothing

replaceInline :: Options -> Scope -> Inline -> WS (ReplacedResult Inline)
replaceInline opts scope (Span ats@(label,_,attrs) [Math DisplayMath eq])
  | Just pfx <- getRefPrefix opts label <|> autoEqnLabels opts
  , Just lbl <- autoLabel pfx label
  = do
      RefRec{..} <- replaceAttr opts scope lbl attrs (B.displayMath eq) pfx
      replaceNoRecurse $ Span ats [Math DisplayMath $ stringify refCaption]
replaceInline opts scope (Image attr@(label,_,attrs) alt img@(_, tit))
  | Just pfx <- getRefPrefix opts label <|> autoFigLabels opts
  , Just lbl <- autoLabel pfx label
  , "fig:" `T.isPrefixOf` tit
  = do
    RefRec{..} <- replaceAttr opts scope lbl attrs (B.fromList alt) pfx
    replaceNoRecurse $ Image attr (B.toList refCaption) img
-- generic span
replaceInline opts scope (Span (label,cls,attrs) content)
  | Just pfx <- getRefPrefix opts label
  = do
      ref@RefRec{} <- replaceAttr opts scope (ExplicitLabel label) attrs (B.fromList content) pfx
      replaceRecurse (newScope ref scope) . Span (label, cls, attrs)
         . B.toList $ applyTitleTemplate ref
replaceInline _opts (scope@RefRec{refPfxRec=Prefix{..}}:_) (Span ("",_,attrs) []) = do
  rd <- get referenceData
  let ccd = filter ((== Just scope) . refScope) . M.elems $ rd
      prefix = maybe mempty B.str $ lookup "prefix" attrs
      suffix = maybe mempty B.str $ lookup "suffix" attrs
      delim = maybe prefixCollectedCaptionDelim B.str $ lookup "delim" attrs
      varFunc rr x = fix defaultVarFunc rr x <|> (MetaString <$> lookup x attrs)
  replaceNoRecurse . Span nullAttr . B.toList $
      prefix <> (
        mconcat
      . intersperse delim
      . map (applyTemplate prefixCollectedCaptionTemplate . varFunc)
      $ sort ccd) <> suffix
replaceInline _ scope _ = noReplaceRecurse scope

applyTitleTemplate :: RefRec -> B.Inlines
applyTitleTemplate rr@RefRec{refPfxRec} =
  applyTemplate (prefixCaptionTemplate refPfxRec) (fix defaultVarFunc rr)

applyTitleIndexTemplate :: RefRec -> B.Inlines
applyTitleIndexTemplate rr@RefRec{..} =
  applyTemplate (prefixCaptionIndexTemplate refPfxRec) vf
  where
  vf "i" = Nothing
  vf "ri" = Just $ MetaInlines $ B.toList refIxInlRaw
  vf x = fix defaultVarFunc rr x

walkReplaceInlines :: [Inline] -> [Inline] -> Block -> Block
walkReplaceInlines newTitle title = walk replaceInlines
  where
  replaceInlines xs
    | xs == title = newTitle
    | otherwise = xs

divBlocks :: Options -> Block -> Block
divBlocks opts (Table tattr@("", _, _) (Caption short (btitle:rest)) colspec header cells foot)
  | [Span (label, cls, attr) title] <- titleWSpan
  , not $ null title
  , isJust $ getRefPrefix opts label
  = Div (label, cls, attr) [Table tattr (Caption short $ walkReplaceInlines title titleWSpan btitle : rest) colspec header cells foot]
  where titleWSpan = blocksToInlines [btitle]
divBlocks opts (Table tattr@("", _, _) (Caption short (btitle:rest)) colspec header cells foot)
  | not $ null title
  , Just label <- getRefLabel opts [last title]
  = Div (label,[],[]) [Table tattr (Caption short $ walkReplaceInlines (dropWhileEnd isSpace (init title)) title btitle : rest) colspec header cells foot]
  where title = blocksToInlines [btitle]
divBlocks opts (CodeBlock (label, classes, attrs) code)
  | Just caption <- lookup "caption" attrs
  , isJust $ getRefPrefix opts label
  = let p   = Para $ Str ":" : Space : B.toList (B.text caption)
        cb' = CodeBlock ("", classes, delete ("caption", caption) attrs) code
    in Div (label, [], []) [cb', p]
divBlocks _ x = x

spanInlines :: Options -> [Inline] -> [Inline]
spanInlines opts (math@(Math DisplayMath _eq):ils)
  | c:ils' <- dropWhile isSpace ils
  , Just label <- getRefLabel opts [c]
  = Span (label,[],[]) [math]:ils'
  | isJust $ autoEqnLabels opts
  = Span nullAttr [math]:ils
spanInlines _ x = x

replaceAttr :: Options -> Scope -> ItemLabel -> [(T.Text, T.Text)] -> B.Inlines -> T.Text -> WS RefRec
replaceAttr o scope label attrs title pfx
  = do
    roptMain <- liftEither $ getPfx o pfx
    let attrMap = M.fromListWith (flip (++)) $ map (second return) attrs
        metaAttrMap = M.map attr2meta attrMap
        attr2meta [s] = MetaString s
        attr2meta ss = MetaList $ map MetaString ss
        scopeSpecifier = fromMaybe (prefixScope ropt) $ M.lookup "scope" attrMap
        itemScope = find ((`elem` scopeSpecifier) . refPfx) scope
        lvl = length $ filter ((== pfx) . refPfx) scope
        ropt = recurseSub lvl roptMain
        recurseSub 0 r = r
        recurseSub l r
          | Just i <- prefixSub r = recurseSub (l-1) i
          | otherwise = r
    cr <- (\CounterRec{..} -> CounterRec{
            crIndex = crIndex+1
          , crIndexInScope = M.insertWith (+) itemScope 1 crIndexInScope
          }) . fromMaybe def . M.lookup pfx <$> get pfxCounter
    modify pfxCounter $ M.insert pfx cr
    let refLabel' = lookup "label" attrs
        label' =
          case label of
            ExplicitLabel l -> l
            AutoLabel -> pfx <> T.pack (':':'\0':show i)
        iInSc = fromJust $ M.lookup itemScope $ crIndexInScope cr
        i = crIndex cr
        customLabel = maybe (prefixNumbering ropt)
                            (mkLabel $ label' <> " attribute numbering")
                      $ M.lookup "numbering" metaAttrMap
    hasLabel <- M.member label' <$> get referenceData
    when hasLabel $ throwError $ WSEDuplicateLabel label'
    let rec' = RefRec {
        refIndex = i
      , refTitle = title
      , refLabel = label'
      , refIxInl = applyTitleIndexTemplate rec'
      , refIxInlRaw = B.text $ fromMaybe (customLabel iInSc) refLabel'
      , refScope = itemScope
      , refLevel = lvl
      , refPfx = pfx
      , refPfxRec = ropt
      , refCaption = applyTitleTemplate rec'
      , refAttrs = (`M.lookup` metaAttrMap)
      , refCaptionPosition = prefixCaptionPosition ropt
      }
    modify referenceData $ M.insert label' rec'
    return rec'

mkCaption :: Options -> T.Text -> B.Inlines -> Block
mkCaption opts style
  | outFormat opts == Just (Format "docx") = Div ("", [], [("custom-style", style)]) . B.toList . B.para
  | otherwise = Para . B.toList
