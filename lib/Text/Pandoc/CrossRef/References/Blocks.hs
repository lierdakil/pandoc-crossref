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

{-# LANGUAGE Rank2Types, MultiWayIf, RecordWildCards, NamedFieldPuns #-}
module Text.Pandoc.CrossRef.References.Blocks
  ( replaceAll
  ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Shared (stringify, hierarchicalize, Element(..))
import Control.Monad.State hiding (get, modify)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M

import Data.Accessor.Monad.Trans.State
import Text.Pandoc.CrossRef.References.Types as Types
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Prefixes
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.CodeBlockCaptions
import Text.Pandoc.CrossRef.Util.VarFunction
import Control.Applicative
import Control.Arrow (second)
import Data.Default (def)
import Prelude

replaceAll :: Options -> [Block] -> WS [Block]
replaceAll opts =
    fmap unhierarchicalize
  . runReplace [] (mkRR (replaceElement opts) `extRR` replaceBlock opts `extRR` replaceInline opts)
  . hierarchicalize
  . runSplitMath
  . everywhere (mkT (divBlocks opts) `extT` spanInlines opts)
  . everywhere (mkT (mkCodeBlockCaptions opts))
  where
    runSplitMath | tableEqns opts
                 , not $ isLatexFormat (outFormat opts)
                 = everywhere (mkT splitMath)
                 | otherwise = id

replaceElement :: Options -> Scope -> Element -> WS (ReplacedResult Element)
replaceElement opts scope (Sec n ns (label, cls, attrs) text' body) = do
  let label' = if autoSectionLabels opts && isNothing pfx
               then "sec:"++label
               else label
      pfx = getRefPrefix opts label
  if "unnumbered" `elem` cls
  then replaceRecurse scope $ Sec n ns (label', cls, attrs) text' body
  else do
    let pfx' = getRefPrefix opts label'
        ititle = B.fromList text'
        defaultSecPfx = "sec"
    rec' <- case pfx' of
      Just p -> replaceAttr opts scope (Right label) attrs ititle p
      Nothing -> replaceAttr opts scope (Left defaultSecPfx) attrs ititle defaultSecPfx
    let title' = B.toList $
          case outFormat opts of
              f | isLatexFormat f -> B.rawInline "latex" (mkLaTeXLabel label) <> ititle
              _  -> applyTitleTemplate opts rec'
    replaceRecurse (newScope rec' scope) $ Sec n ns (label', cls, attrs) title' body
replaceElement _ scope _ = noReplaceRecurse scope

replaceBlock :: Options -> Scope -> Block -> WS (ReplacedResult Block)
replaceBlock opts scope (Div divOps@(label,_,attrs) [Table title align widths header cells])
  | not $ null title
  , Just pfx <- getRefPrefix opts label
  = do
    let ititle = B.fromList title
    idxStr <- replaceAttr opts scope (Right label) attrs ititle pfx
    let title' = B.toList $
          case outFormat opts of
              f | isLatexFormat f -> B.rawInline "latex" (mkLaTeXLabel label) <> ititle
              _  -> applyTitleTemplate opts idxStr
    replaceNoRecurse $ Div divOps [Table title' align widths header cells]
replaceBlock opts scope (Div (label,"listing":_, []) [Para caption, CodeBlock ([],classes,attrs) code])
  | not $ null label
  , Just pfx <- getRefPrefix opts label
  = do
      let icaption = B.fromList caption
      idxStr <- replaceAttr opts scope (Right label) attrs icaption pfx
      let caption' = applyTitleTemplate opts idxStr
      replaceNoRecurse $ case outFormat opts of
        f --if used with listings package, return code block with caption
          | isLatexFormat f, listings opts ->
            CodeBlock (label,classes,("caption",stringify caption):attrs) code
          --if not using listings, however, wrap it in a codelisting environment
          | isLatexFormat f ->
            Div nullAttr [
                RawBlock (Format "latex") "\\begin{codelisting}"
              , Para [
                  RawInline (Format "latex") "\\caption"
                , Span nullAttr caption
                ]
              , CodeBlock (label,classes,attrs) code
              , RawBlock (Format "latex") "\\end{codelisting}"
              ]
        _ -> Div (label, "listing":classes, []) [
            mkCaption opts "Caption" caption'
          , CodeBlock ([], classes, attrs) code
          ]
replaceBlock opts scope (Para [Span attrs@(label, _, _) [Math DisplayMath eq]])
  | not $ isLatexFormat (outFormat opts)
  , tableEqns opts
  , pfx <- getRefPrefix opts label
  = do
    (eq', idx) <- replaceEqn opts scope attrs eq pfx
    replaceNoRecurse $ Div attrs [Table [] [AlignCenter, AlignRight] [0.9, 0.09] [] [[[Plain [Math DisplayMath eq']], [Plain [Math DisplayMath $ "(" ++ idx ++ ")"]]]]]
replaceBlock opts scope x@(Div (label, _, attrs) _content)
  | Just pfx <- getRefPrefix opts label
  = do
    rec' <- replaceAttr opts scope (Right label) attrs mempty pfx
    replaceRecurse (newScope rec' scope) x
replaceBlock _ scope _ = noReplaceRecurse scope

replaceEqn :: Options -> Scope -> Attr -> String -> Maybe String -> WS (String, String)
replaceEqn opts scope (label, _, attrs) eq pfx = do
  let label' | null label = Left "eq"
             | otherwise = Right label
  idxStr <- replaceAttr opts scope label' attrs (B.math eq) (fromMaybe "eq" pfx)
  let eq' | tableEqns opts = eq
          | otherwise = eq++"\\qquad("++stringify (refIxInl idxStr)++")"
  return (eq', stringify (refIxInl idxStr))

replaceInline :: Options -> Scope -> Inline -> WS (ReplacedResult Inline)
replaceInline opts scope (Span attrs@(label,_,_) [Math DisplayMath eq])
  | pfx <- getRefPrefix opts label
  , isJust pfx || null label && autoEqnLabels opts
  = do
      (eq', _) <- replaceEqn opts scope attrs eq pfx
      replaceNoRecurse $ case outFormat opts of
        f | isLatexFormat f ->
          let eqn = "\\begin{equation}"++eq++mkLaTeXLabel label++"\\end{equation}"
          in RawInline (Format "latex") eqn
        _ -> Span attrs [Math DisplayMath eq']
replaceInline opts scope (Image attr@(label,_,attrs) alt img@(_, tit))
  | Just pfx <- getRefPrefix opts label
  , "fig:" `isPrefixOf` tit
  = do
    let ialt = B.fromList alt
    idxStr <- replaceAttr opts scope (Right label) attrs ialt pfx
    let alt' = B.toList $ case outFormat opts of
          f | isLatexFormat f -> ialt
          _  -> applyTitleTemplate opts idxStr
    replaceNoRecurse $ Image attr alt' img
replaceInline opts scope x@(Span (label,_,attrs) content)
  | Just pfx <- getRefPrefix opts label
  = do
      rec' <- replaceAttr opts scope (Right label) attrs (B.fromList content) pfx
      replaceRecurse (newScope rec' scope) x
replaceInline _ scope _ = noReplaceRecurse scope

applyTitleTemplate :: Options -> RefRec -> B.Inlines
applyTitleTemplate opts rr@RefRec{refPfx} =
  applyTemplate (pfxCaptionTemplate opts refPfx) (fix defaultVarFunc rr)

applyTitleIndexTemplate :: Options -> RefRec -> B.Inlines -> B.Inlines
applyTitleIndexTemplate opts rr@RefRec{..} label =
  applyTemplate (pfxCaptionIndexTemplate opts refPfx) vf
  where
  vf "i" = Just $ MetaInlines $ B.toList label
  vf x = fix defaultVarFunc rr x

divBlocks :: Options -> Block -> Block
divBlocks opts (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel opts [last title]
  = Div (label,[],[]) [Table (dropWhileEnd isSpace $ init title) align widths header cells]
divBlocks opts (CodeBlock (label, classes, attrs) code)
  | Just caption <- lookup "caption" attrs
  , Just _ <- getRefPrefix opts label
  = let p   = Para $ B.toList $ B.text caption
        cb' = CodeBlock ([], classes, delete ("caption", caption) attrs) code
    in Div (label,"listing":classes, []) [p, cb']
divBlocks _ x = x

splitMath :: [Block] -> [Block]
splitMath (Para ils:xs)
  | length ils > 1 = map Para (split [] [] ils) ++ xs
  where
    split res acc [] = reverse (reverse acc : res)
    split res acc (x@(Span _ [Math DisplayMath _]):ys) =
      split ([x] : reverse (dropSpaces acc) : res)
            [] (dropSpaces ys)
    split res acc (y:ys) = split res (y:acc) ys
    dropSpaces = dropWhile isSpace
splitMath xs = xs

spanInlines :: Options -> [Inline] -> [Inline]
spanInlines opts (math@(Math DisplayMath _eq):ils)
  | c:ils' <- dropWhile isSpace ils
  , Just label <- getRefLabel opts [c]
  = Span (label,[],[]) [math]:ils'
  | autoEqnLabels opts
  = Span nullAttr [math]:ils
spanInlines _ x = x

replaceAttr :: Options -> Scope -> Either String String -> [(String, String)] -> B.Inlines -> String -> WS RefRec
replaceAttr o scope label attrs title pfx
  = do
    let ropt = getPfx o pfx
        itemScope = find ((`elem` prefixScope ropt) . refPfx) scope
        refLabel' = lookup "label" attrs
    cr <- (\CounterRec{..} -> CounterRec{
            crIndex = crIndex+1
          , crIndexInScope = M.insertWith (+) itemScope 1 crIndexInScope
          }) . fromMaybe def . M.lookup pfx <$> get pfxCounter
    modify pfxCounter $ M.insert pfx cr
    let label' = either (++ ':':'\0':show i) id label
        iInSc = fromJust $ M.lookup itemScope $ crIndexInScope cr
        i = crIndex cr
        lvl = length $ filter ((== pfx) . refPfx) scope
        customLabel = prefixNumbering ropt lvl
    hasLabel <- M.member label' <$> get referenceData
    when hasLabel $ error $ "Duplicate label: " ++ label'
    let rec' = RefRec {
        refIndex = i
      , refTitle = title
      , refLabel = label'
      , refIxInl = applyTitleIndexTemplate o rec' $ B.text $ fromMaybe (customLabel iInSc) refLabel'
      , refScope = itemScope
      , refLevel = lvl
      , refPfx = pfx
      , refCaption = applyTitleTemplate o rec'
      , refAttrs = M.fromListWith (flip (++)) $ map (second return) attrs
      }
    modify referenceData $ M.insert label' rec'
    return rec'

mkCaption :: Options -> String -> B.Inlines -> Block
mkCaption opts style
  | outFormat opts == Just (Format "docx") = Div ([], [], [("custom-style", style)]) . B.toList . B.para
  | otherwise = Para . B.toList
