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

{-# LANGUAGE Rank2Types, MultiWayIf #-}
module Text.Pandoc.CrossRef.References.Blocks
  ( replaceAll
  ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Shared (stringify)
import Control.Monad.State hiding (get, modify)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M

import Data.Accessor
import Data.Accessor.Monad.Trans.State
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Control.Applicative
import Prelude
import Data.Default

replaceAll :: (Data a) => Options -> a -> WS a
replaceAll opts =
    runReplace (mkRR (replaceBlock opts) `extRR` replaceInline opts)
  . runSplitMath
  . everywhere (mkT divBlocks `extT` spanInlines opts)
  where
    runSplitMath | tableEqns opts
                 , not $ isLatexFormat (outFormat opts)
                 = everywhere (mkT splitMath)
                 | otherwise = id

replaceBlock :: Options -> Block -> WS (ReplacedResult Block)
replaceBlock opts (Header n (label, cls, attrs) text')
  = do
    let label' = if autoSectionLabels opts && not ("sec:" `isPrefixOf` label)
                 then "sec:"++label
                 else label
    unless ("unnumbered" `elem` cls) $ do
      modify curChap $ \cc ->
        let ln = length cc
            cl = lookup "label" attrs
            inc l = init l ++ [(fst (last l) + 1, cl)]
            cc' | ln > n = inc $ take n cc
                | ln == n = inc cc
                | otherwise = cc ++ take (n-ln-1) (zip [1,1..] $ repeat Nothing) ++ [(1,cl)]
        in cc'
      when ("sec:" `isPrefixOf` label') $ do
        index  <- get curChap
        modify secRefs $ M.insert label' RefRec {
          refIndex=index
        , refTitle= text'
        , refSubfigure = Nothing
        }
    cc <- get curChap
    let textCC | numberSections opts
               , sectionsDepth opts < 0 || n <= if sectionsDepth opts == 0 then chaptersDepth opts else sectionsDepth opts
               , "unnumbered" `notElem` cls
               = Str (intercalate "." $ map show' cc) : Space : text'
               | otherwise = text'
        show' (_, Just s) = s
        show' (i, Nothing) = show i
    replaceNoRecurse $ Header n (label', cls, attrs) textCC
-- subfigures
replaceBlock opts (Div (label,cls,attrs) images)
  | "fig:" `isPrefixOf` label
  , Para caption <- last images
  = do
    idxStr <- replaceAttr opts (Right label) (lookup "label" attrs) caption imgRefs
    let (cont, st) = runState (runReplace (mkRR $ replaceSubfigs opts') $ init images) def
        collectedCaptions = B.toList $
            intercalate' (B.fromList $ ccsDelim opts)
          $ map (B.fromList . collectCaps . snd)
          $ sortOn (refIndex . snd)
          $ filter (not . null . refTitle . snd)
          $ M.toList
          $ imgRefs_ st
        collectCaps v =
              applyTemplate
                (chapPrefix (chapDelim opts) (refIndex v))
                (refTitle v)
                (ccsTemplate opts)
        vars = M.fromDistinctAscList
                  [ ("ccs", collectedCaptions)
                  , ("i", idxStr)
                  , ("t", caption)
                  ]
        capt = applyTemplate' vars $ subfigureTemplate opts
    lastRef <- fromJust . M.lookup label <$> get imgRefs
    modify imgRefs $ \old ->
        M.union
          old
          (M.map (\v -> v{refIndex = refIndex lastRef, refSubfigure = Just $ refIndex v})
          $ imgRefs_ st)
    case outFormat opts of
          f | isLatexFormat f ->
            replaceNoRecurse $ Div nullAttr $
              [ RawBlock (Format "latex") "\\begin{figure}\n\\centering" ]
              ++ cont ++
              [ Para [RawInline (Format "latex") "\\caption"
                       , Span nullAttr caption]
              , RawBlock (Format "latex") $ mkLaTeXLabel label
              , RawBlock (Format "latex") "\\end{figure}"]
          _  -> replaceNoRecurse $ Div (label, "subfigures":cls, attrs) $ toTable cont capt
  where
    opts' = opts
              { figureTemplate = subfigureChildTemplate opts
              , customLabel = \r i -> customLabel opts ("sub"++r) i
              }
    toTable :: [Block] -> [Inline] -> [Block]
    toTable blks capt
      | subfigGrid opts = [Table [] align widths [] $ map blkToRow blks, mkCaption opts "Image Caption" capt]
      | otherwise = blks ++ [mkCaption opts "Image Caption" capt]
      where
        align | Para ils:_ <- blks = replicate (length $ mapMaybe getWidth ils) AlignCenter
              | otherwise = error "Misformatted subfigures block"
        widths | Para ils:_ <- blks
               = fixZeros $ mapMaybe getWidth ils
               | otherwise = error "Misformatted subfigures block"
        getWidth (Image (_id, _class, as) _ _)
          = Just $ maybe 0 percToDouble $ lookup "width" as
        getWidth _ = Nothing
        fixZeros :: [Double] -> [Double]
        fixZeros ws
          = let nz = length $ filter (== 0) ws
                rzw = (0.99 - sum ws) / fromIntegral nz
            in if nz>0
               then map (\x -> if x == 0 then rzw else x) ws
               else ws
        percToDouble :: String -> Double
        percToDouble percs
          | '%' <- last percs
          , perc <- read $ init percs
          = perc/100.0
          | otherwise = error "Only percent allowed in subfigure width!"
        blkToRow :: Block -> [[Block]]
        blkToRow (Para inls) = mapMaybe inlToCell inls
        blkToRow x = [[x]]
        inlToCell :: Inline -> Maybe [Block]
        inlToCell (Image (id', cs, as) txt tgt)  = Just [Para [Image (id', cs, setW as) txt tgt]]
        inlToCell _ = Nothing
        setW as = ("width", "100%"):filter ((/="width") . fst) as
replaceBlock opts (Div divOps@(label,_,attrs) [Table title align widths header cells])
  | not $ null title
  , "tbl:" `isPrefixOf` label
  = do
    idxStr <- replaceAttr opts (Right label) (lookup "label" attrs) title tblRefs
    let title' =
          case outFormat opts of
              f | isLatexFormat f ->
                RawInline (Format "latex") (mkLaTeXLabel label) : title
              _  -> applyTemplate idxStr title $ tableTemplate opts
    replaceNoRecurse $ Div divOps [Table title' align widths header cells]
replaceBlock opts cb@(CodeBlock (label, classes, attrs) code)
  | not $ null label
  , "lst:" `isPrefixOf` label
  , Just caption <- lookup "caption" attrs
  = case outFormat opts of
      f
        --if used with listings package,nothing shoud be done
        | isLatexFormat f, listings opts -> noReplaceNoRecurse
        --if not using listings, however, wrap it in a codelisting environment
        | isLatexFormat f ->
          replaceNoRecurse $ Div nullAttr [
              RawBlock (Format "latex") "\\begin{codelisting}"
            , Plain [
                RawInline (Format "latex") "\\caption{"
              , Str caption
              , RawInline (Format "latex") "}"
              ]
            , cb
            , RawBlock (Format "latex") "\\end{codelisting}"
            ]
      _ -> do
        let cap = B.toList $ B.text caption
        idxStr <- replaceAttr opts (Right label) (lookup "label" attrs) cap lstRefs
        let caption' = applyTemplate idxStr cap $ listingTemplate opts
        replaceNoRecurse $ Div (label, "listing":classes, []) [
            mkCaption opts "Caption" caption'
          , CodeBlock ([], classes, attrs \\ [("caption", caption)]) code
          ]
replaceBlock opts
  (Div (label,"listing":_, [])
    [Para caption, CodeBlock ([],classes,attrs) code])
  | not $ null label
  , "lst:" `isPrefixOf` label
  = case outFormat opts of
      f
        --if used with listings package, return code block with caption
        | isLatexFormat f, listings opts ->
          replaceNoRecurse $ CodeBlock (label,classes,("caption",stringify caption):attrs) code
        --if not using listings, however, wrap it in a codelisting environment
        | isLatexFormat f ->
          replaceNoRecurse $ Div nullAttr [
              RawBlock (Format "latex") "\\begin{codelisting}"
            , Para [
                RawInline (Format "latex") "\\caption"
              , Span nullAttr caption
              ]
            , CodeBlock (label,classes,attrs) code
            , RawBlock (Format "latex") "\\end{codelisting}"
            ]
      _ -> do
        idxStr <- replaceAttr opts (Right label) (lookup "label" attrs) caption lstRefs
        let caption' = applyTemplate idxStr caption $ listingTemplate opts
        replaceNoRecurse $ Div (label, "listing":classes, []) [
            mkCaption opts "Caption" caption'
          , CodeBlock ([], classes, attrs) code
          ]
replaceBlock opts (Para [Span attrs [Math DisplayMath eq]])
  | not $ isLatexFormat (outFormat opts)
  , tableEqns opts
  = do
    (eq', idx) <- replaceEqn opts attrs eq
    replaceNoRecurse $ Div attrs [Table [] [AlignCenter, AlignRight] [0.9, 0.09] [] [[[Plain [Math DisplayMath eq']], [Plain [Math DisplayMath $ "(" ++ idx ++ ")"]]]]]
replaceBlock _ _ = noReplaceRecurse

replaceEqn :: Options -> Attr -> String -> WS (String, String)
replaceEqn opts (label, _, attrs) eq = do
  let label' | null label = Left "eq"
             | otherwise = Right label
  idxStr <- replaceAttr opts label' (lookup "label" attrs) [] eqnRefs
  let eq' | tableEqns opts = eq
          | otherwise = eq++"\\qquad("++stringify idxStr++")"
  return (eq', stringify idxStr)

replaceInline :: Options -> Inline -> WS (ReplacedResult Inline)
replaceInline opts (Span attrs@(label,_,_) [Math DisplayMath eq])
  | "eq:" `isPrefixOf` label || null label && autoEqnLabels opts
  = case outFormat opts of
      f | isLatexFormat f ->
        let eqn = "\\begin{equation}"++eq++mkLaTeXLabel label++"\\end{equation}"
        in replaceNoRecurse $ RawInline (Format "latex") eqn
      _ -> do
        (eq', _) <- replaceEqn opts attrs eq
        replaceNoRecurse $ Span attrs [Math DisplayMath eq']
replaceInline opts (Image attr@(label,_,attrs) alt img@(_, tit))
  | "fig:" `isPrefixOf` label && "fig:" `isPrefixOf` tit
  = do
    idxStr <- replaceAttr opts (Right label) (lookup "label" attrs) alt imgRefs
    let alt' = case outFormat opts of
          f | isLatexFormat f -> alt
          _  -> applyTemplate idxStr alt $ figureTemplate opts
    replaceNoRecurse $ Image attr alt' img
replaceInline _ _ = noReplaceRecurse

replaceSubfigs :: Options -> [Inline] -> WS (ReplacedResult [Inline])
replaceSubfigs opts = (replaceNoRecurse . concat =<<) . mapM (replaceSubfig opts)

replaceSubfig :: Options -> Inline -> WS [Inline]
replaceSubfig opts x@(Image (label,cls,attrs) alt (src, tit))
  = do
      let label' | "fig:" `isPrefixOf` label = Right label
                 | null label = Left "fig"
                 | otherwise  = Right $ "fig:" ++ label
      idxStr <- replaceAttr opts label' (lookup "label" attrs) alt imgRefs
      case outFormat opts of
        f | isLatexFormat f ->
          return $ latexSubFigure x label
        _  ->
          let alt' = applyTemplate idxStr alt $ figureTemplate opts
              tit' | "nocaption" `elem` cls = fromMaybe tit $ stripPrefix "fig:" tit
                   | "fig:" `isPrefixOf` tit = tit
                   | otherwise = "fig:" ++ tit
          in return [Image (label, cls, attrs) alt' (src, tit')]
replaceSubfig _ x = return [x]

divBlocks :: Block -> Block
divBlocks (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = Div (label,[],[]) [Table (dropWhileEnd isSpace $ init title) align widths header cells]
  where isSpace = (||) <$> (==Space) <*> (==SoftBreak)
divBlocks x = x


splitMath :: [Block] -> [Block]
splitMath (Para ils:xs)
  | length ils > 1 = map Para (split [] [] ils) ++ xs
  where
    split res acc [] = reverse (reverse acc : res)
    split res acc (x@(Span _ [Math DisplayMath _]):ys) =
      split ([x] : reverse (dropSpaces acc) : res)
            [] (dropSpaces ys)
    split res acc (y:ys) = split res (y:acc) ys
    dropSpaces = dropWhile (\x -> x == Space || x == SoftBreak)
splitMath xs = xs

spanInlines :: Options -> [Inline] -> [Inline]
spanInlines opts (math@(Math DisplayMath _eq):ils)
  | c:ils' <- dropWhile (==Space) ils
  , Just label <- getRefLabel "eq" [c]
  = Span (label,[],[]) [math]:ils'
  | autoEqnLabels opts
  = Span nullAttr [math]:ils
spanInlines _ x = x

replaceAttr :: Options -> Either String String -> Maybe String -> [Inline] -> Accessor References RefMap -> WS [Inline]
replaceAttr o label refLabel title prop
  = do
    chap  <- take (chaptersDepth o) `fmap` get curChap
    i     <- (1+) `fmap` (M.size . M.filter (\x -> (chap == init (refIndex x)) && isNothing (refSubfigure x)) <$> get prop)
    let index = chap ++ [(i, refLabel <> customLabel o label' i)]
        label' = either (++ ':':show index) id label
    hasLabel <- M.member label' <$> get prop
    when hasLabel $
      error $ "Duplicate label: " ++ label'
    modify prop $ M.insert label' RefRec {
      refIndex= index
    , refTitle= title
    , refSubfigure = Nothing
    }
    return $ chapPrefix (chapDelim o) index

latexSubFigure :: Inline -> String -> [Inline]
latexSubFigure (Image (_, cls, attrs) alt (src, title)) label =
  let
    title' = fromMaybe title $ stripPrefix "fig:" title
    texlabel | null label = []
             | otherwise = [RawInline (Format "latex") $ mkLaTeXLabel label]
    texalt | "nocaption" `elem` cls  = []
           | otherwise = concat
              [ [ RawInline (Format "latex") "["]
              , alt
              , [ RawInline (Format "latex") "]"]
              ]
    img = Image (label, cls, attrs) alt (src, title')
  in concat [
      [ RawInline (Format "latex") "\\subfloat" ]
      , texalt
      , [Span nullAttr $ img:texlabel]
      ]
latexSubFigure x _ = [x]

mkCaption :: Options -> String -> [Inline] -> Block
mkCaption opts style
  | outFormat opts == Just (Format "docx") = Div ([], [], [("custom-style", style)]) . return . Para
  | otherwise = Para
