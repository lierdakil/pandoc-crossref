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

{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeFamilies #-}
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

replaceAll :: Options -> [Block] -> WS [Block]
replaceAll opts =
    fmap unhierarchicalize
  . runReplace [] (mkRR (replaceElement opts) `extRR` replaceBlock opts `extRR` replaceInline opts)
  . hierarchicalize
  . runSplitMath
  . everywhere (mkT $ makeSubfigures opts)
  . everywhere (mkT (divBlocks opts) `extT` spanInlines opts)
  . everywhere (mkT $ mkCodeBlockCaptions opts)
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
      Just p -> replaceAttr opts scope (Right label') attrs ititle p
      Nothing -> replaceAttr opts scope (Left defaultSecPfx) attrs ititle defaultSecPfx
    let title' = B.toList $ refCaption rec'
    replaceRecurse (newScope rec' scope) $ Sec n ns (label', cls, attrs) title' body
replaceElement _ scope _ = noReplaceRecurse scope

replaceBlock :: Options -> Scope -> Block -> WS (ReplacedResult Block)
-- tables
replaceBlock opts scope (Div divOps@(label,_,attrs) [Table title align widths header cells])
  | not $ null title
  , Just pfx <- getRefPrefix opts label
  = do
    let ititle = B.fromList title
    idxStr <- replaceAttr opts scope (Right label) attrs ititle pfx
    let title' = B.toList $
          case outFormat opts of
              f | isLatexFormat f -> B.rawInline "latex" (mkLaTeXLabel label) <> ititle
              _  -> refCaption idxStr
    replaceNoRecurse $ Div divOps [Table title' align widths header cells]
-- code blocks
replaceBlock opts scope (Div (label, [], divattrs) [CodeBlock ([],classes,cbattrs) code, Para (Str ":":Space:caption)])
  | Just pfx <- getRefPrefix opts label
  = do
      idxStr <- replaceAttr opts scope (Right label) divattrs (B.fromList caption) pfx
      replaceNoRecurse $ case outFormat opts of
        f --if used with listings package, return code block with caption
          | isLatexFormat f, listings opts ->
            CodeBlock (label,classes,("caption",stringify caption):cbattrs) code
          --if not using listings, however, wrap it in a codelisting environment
          | isLatexFormat f ->
            Div nullAttr [
                RawBlock (Format "latex") "\\begin{codelisting}"
              , Plain [
                  RawInline (Format "latex") "\\caption"
                , Span nullAttr caption
                ]
              , CodeBlock (label,classes,cbattrs) code
              , RawBlock (Format "latex") "\\end{codelisting}"
              ]
        _ -> Div (label, "listing":classes, []) $ placeCaption opts idxStr
              [CodeBlock ([], classes, cbattrs) code]
-- Table display math
replaceBlock opts scope (Para [Span ats@(label, _, attrs) [Math DisplayMath eq]])
  | not $ isLatexFormat (outFormat opts)
  , tableEqns opts
  , Just pfx <- getRefPrefix opts label <|> autoEqnLabels opts
  , Just lbl <- autoLabel pfx label
  = do
    (eq', idx) <- replaceEqn opts scope lbl attrs eq pfx
    replaceNoRecurse $ Div ats [Table [] [AlignCenter, AlignRight] [0.9, 0.09] [] [[[Plain [Math DisplayMath eq']], [Plain [Math DisplayMath $ "(" ++ idx ++ ")"]]]]]
-- Generic div
replaceBlock opts scope x@(Div ats@(label, _, attrs) content)
  | Just pfx <- getRefPrefix opts label
  = do
    let caption
          | not (null content)
          , Para (Str ":":Space:c) <- last content
          = Just $ B.fromList c
          | otherwise = Nothing
    rec' <- replaceAttr opts scope (Right label) attrs (fromMaybe mempty caption) pfx
    replaceRecurse (newScope rec' scope) $ case caption of
      Nothing -> x
      Just _ -> Div ats . placeCaption opts rec' $ init content
replaceBlock _ scope _ = noReplaceRecurse scope

placeCaption :: Options -> RefRec -> [Block] -> [Block]
placeCaption opts RefRec{..} body
  | Above <- refCaptionPosition
  = mkCaption opts "Caption" refCaption : body
  | Below <- refCaptionPosition
  = body <> [mkCaption opts "Caption" refCaption]

replaceEqn :: Options -> Scope -> Either String String -> [(String, String)] -> String -> String -> WS (String, String)
replaceEqn opts scope label attrs eq pfx = do
  idxStr <- replaceAttr opts scope label attrs (B.math eq) pfx
  let eq' | tableEqns opts = eq
          | otherwise = eq++"\\qquad("++stringify (refIxInl idxStr)++")"
  return (eq', stringify (refIxInl idxStr))

autoLabel :: String -> String -> Maybe (Either String String)
autoLabel pfx label
  | null label = Just $ Left pfx
  | (pfx <> ":") `isPrefixOf` label = Just $ Right label
  | otherwise = Nothing

replaceInline :: Options -> Scope -> Inline -> WS (ReplacedResult Inline)
replaceInline opts scope (Span ats@(label,_,attrs) [Math DisplayMath eq])
  | Just pfx <- getRefPrefix opts label <|> autoEqnLabels opts
  , Just lbl <- autoLabel pfx label
  = do
      (eq', _) <- replaceEqn opts scope lbl attrs eq pfx
      replaceNoRecurse $ case outFormat opts of
        f | isLatexFormat f ->
          RawInline (Format "latex")
          $ "\\begin{equation}"++eq++mkLaTeXLabel label++"\\end{equation}"
        _ -> Span ats [Math DisplayMath eq']
replaceInline opts scope (Image attr@(label,_,attrs) alt img@(_, tit))
  | Just pfx <- getRefPrefix opts label <|> autoFigLabels opts
  , Just lbl <- autoLabel pfx label
  , "fig:" `isPrefixOf` tit
  = do
    let ialt = B.fromList alt
    idxStr <- replaceAttr opts scope lbl attrs ialt pfx
    let alt' = B.toList $ case outFormat opts of
          f | isLatexFormat f -> ialt
          _  -> refCaption idxStr
    replaceNoRecurse $ Image attr alt' img
replaceInline opts scope (Span (label,_,attrs) content)
  | Just pfx <- getRefPrefix opts label
  = do
      rec' <- replaceAttr opts scope (Right label) attrs (B.fromList content) pfx
      noReplaceRecurse (newScope rec' scope)
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

divBlocks :: Options -> Block -> Block
divBlocks opts (Table [Span (label, cls, attr) title] align widths header cells)
  | not $ null title
  , isJust $ getRefPrefix opts label
  = Div (label, cls, attr) [Table (dropWhileEnd isSpace $ init title) align widths header cells]
divBlocks opts (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel opts [last title]
  = Div (label,[],[]) [Table (dropWhileEnd isSpace $ init title) align widths header cells]
divBlocks opts (CodeBlock (label, classes, attrs) code)
  | Just caption <- lookup "caption" attrs
  , isJust $ getRefPrefix opts label
  = let p   = Para $ Str ":" : Space : B.toList (B.text caption)
        cb' = CodeBlock ([], classes, delete ("caption", caption) attrs) code
    in Div (label, [], []) [cb', p]
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
  | isJust $ autoEqnLabels opts
  = Span nullAttr [math]:ils
spanInlines _ x = x

replaceAttr :: Options -> Scope -> Either String String -> [(String, String)] -> B.Inlines -> String -> WS RefRec
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
        label' = either (++ ':':'\0':show i) id label
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
      , refAttrs = metaAttrMap
      , refCaptionPosition = prefixCaptionPosition ropt
      }
    modify referenceData $ M.insert label' rec'
    return rec'

mkCaption :: Options -> String -> B.Inlines -> Block
mkCaption opts style
  | outFormat opts == Just (Format "docx") = Div ([], [], [("custom-style", style)]) . B.toList . B.para
  | otherwise = Para . B.toList
