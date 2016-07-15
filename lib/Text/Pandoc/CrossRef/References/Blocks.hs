{-# LANGUAGE Rank2Types, MultiWayIf #-}
module Text.Pandoc.CrossRef.References.Blocks
  ( replaceAll
  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.Shared (stringify, normalizeSpaces)
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

replaceAll :: Data a => Options -> a -> WS a
replaceAll opts =
    fmap (everywhere' (mkT clearStopAttrDivs `extT` clearStopAttrSpans))
  . everywhereMBut' (mkQ False isSubfig `extQ` isSubfig') (mkM (replaceBlocks opts) `extM` replaceInlines opts)
  . everywhere' (mkT divBlocks `extT` spanInlines)
  where
    isSubfig (Div (label,cls,_) _)
      | "fig:" `isPrefixOf` label = True
      | "crossref-stop" `elem` cls = True
    isSubfig _ = False
    isSubfig' (Span (_,cls,_) _)
      | "crossref-stop" `elem` cls = True
    isSubfig' _ = False

clearStopAttrSpans :: [Inline] -> [Inline]
clearStopAttrSpans (Span ([], ["crossref-stop"], []) ils:xs)
  = ils ++ xs
clearStopAttrSpans xs = xs

clearStopAttrDivs :: [Block] -> [Block]
clearStopAttrDivs (Div ([], ["crossref-stop"], []) bls:xs)
  = bls ++ xs
clearStopAttrDivs xs = xs

replaceBlocks :: Options -> Block -> WS Block
replaceBlocks opts (Header n (label, cls, attrs) text')
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
      when ("sec:" `isPrefixOf` label') $ replaceAttrSec label' text' secRefs
    return $ Header n (label', cls, attrs) text'
-- subfigures
replaceBlocks opts (Div (label,cls,attrs) images)
  | "fig:" `isPrefixOf` label
  , Para caption <- last images
  = do
    idxStr <- replaceAttr opts (Just label) (lookup "label" attrs) caption imgRefs
    let (cont, st) = runState (replaceAll opts' $ init images) (subFig ^= True $ def)
        collectedCaptions =
            intercalate (ccsDelim opts)
          $ map snd
          $ M.toList
          $ M.map collectCaps
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
          f | isFormat "latex" f ->
            return $ Div stopAttr $
              [ RawBlock (Format "tex") "\\begin{figure}" ]
              ++ cont ++
              [ Para [RawInline (Format "tex") "\\caption"
                       , Span stopAttr [Span nullAttr caption]] -- stopAttr will be stripped
              , RawBlock (Format "tex") $ mkLaTeXLabel label
              , RawBlock (Format "tex") "\\end{figure}"]
          _  -> return $ Div (label, "subfigures":cls, attrs) $ toTable cont capt
  where
    opts' = opts
              { figureTemplate = subfigureChildTemplate opts
              , customLabel = \r i -> customLabel opts ("sub"++r) i
              }
    toTable :: [Block] -> [Inline] -> [Block]
    toTable blks capt
      | subfigGrid opts = [Table [] align widths [] $ map blkToRow blks, Para capt]
      | otherwise = blks ++ [Para capt]
      where
        align | Para ils:_ <- blks = replicate (length $ mapMaybe getWidth ils) AlignCenter
              | otherwise = error "Misformatted subfigures block"
        widths | Para ils:_ <- blks
               = fixZeros $ mapMaybe getWidth ils
               | otherwise = error "Misformatted subfigures block"
        getWidth (Image (_id, _class, as) _ _)
          = Just $ maybe 0 percToDouble $ lookup "width" as
        getWidth _ = Nothing
        fixZeros ws
          = let (lz, lnz) = partition (== 0) ws
                nz = length lz
                sw = sum lnz
                rzw = (0.99 - sw) / fromIntegral nz
                rep [] = []
                rep (0:xs) = rzw:rep xs
                rep (x:xs) = x:rep xs
            in rep ws
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

replaceBlocks opts (Div (label,_,attrs) [Table title align widths header cells])
  | not $ null title
  , "tbl:" `isPrefixOf` label
  = do
    idxStr <- replaceAttr opts (Just label) (lookup "label" attrs) title tblRefs
    let title' =
          case outFormat opts of
              f | isFormat "latex" f ->
                RawInline (Format "tex") (mkLaTeXLabel label) : title
              _  -> applyTemplate idxStr title $ tableTemplate opts
    return $ Table title' align widths header cells
replaceBlocks opts cb@(CodeBlock (label, classes, attrs) code)
  | not $ null label
  , "lst:" `isPrefixOf` label
  , Just caption <- lookup "caption" attrs
  = case outFormat opts of
      f
        --if used with listings package,nothing shoud be done
        | isFormat "latex" f, listings opts -> return cb
        --if not using listings, however, wrap it in a codelisting environment
        | isFormat "latex" f ->
          return $ Div stopAttr [
              RawBlock (Format "tex")
                $ "\\begin{codelisting}\n\\caption{"++caption++"}"
            , cb
            , RawBlock (Format "tex") "\\end{codelisting}"
            ]
      _ -> do
        let cap = toList $ text caption
        idxStr <- replaceAttr opts (Just label) (lookup "label" attrs) cap lstRefs
        let caption' = applyTemplate idxStr cap $ listingTemplate opts
        return $ Div (label, "listing":classes, []) [
            Para caption'
          , CodeBlock ([], classes, attrs \\ [("caption", caption)]) code
          ]
replaceBlocks opts
  (Div (label,"listing":_, [])
    [Para caption, CodeBlock ([],classes,attrs) code])
  | not $ null label
  , "lst:" `isPrefixOf` label
  = case outFormat opts of
      f
        --if used with listings package, return code block with caption
        | isFormat "latex" f, listings opts ->
          return $ CodeBlock (label,classes,("caption",stringify caption):attrs) code
        --if not using listings, however, wrap it in a codelisting environment
        | isFormat "latex" f ->
          return $ Div stopAttr [
              RawBlock (Format "tex") "\\begin{codelisting}"
            , Para [
                RawInline (Format "tex") "\\caption"
              , Span stopAttr [Span nullAttr caption] -- stopAttr will be stripped
              ]
            , CodeBlock (label,classes,attrs) code
            , RawBlock (Format "tex") "\\end{codelisting}"
            ]
      _ -> do
        idxStr <- replaceAttr opts (Just label) (lookup "label" attrs) caption lstRefs
        let caption' = applyTemplate idxStr caption $ listingTemplate opts
        return $ Div (label, "listing":classes, []) [
            Para caption'
          , CodeBlock ([], classes, attrs) code
          ]
replaceBlocks opts (Para [Span (label, _, attrs) [Math DisplayMath eq]])
  | not $ isFormat "latex" (outFormat opts)
  , tableEqns opts
  = do
    idxStr <- replaceAttr opts (Just label) (lookup "label" attrs) [] eqnRefs
    return $ Div stopAttr [Table [] [AlignCenter, AlignRight] [0.9, 0.09] [] [[[Plain [Math DisplayMath eq]], [Plain [Math DisplayMath $ "(" ++ stringify idxStr ++ ")"]]]]]
replaceBlocks _ x = return x

replaceInlines :: Options -> Inline -> WS Inline
replaceInlines opts (Span (label,_,attrs) [Math DisplayMath eq])
  | "eq:" `isPrefixOf` label
  = case outFormat opts of
      f | isFormat "latex" f ->
        let eqn = "\\begin{equation}"++eq++mkLaTeXLabel label++"\\end{equation}"
        in return $ RawInline (Format "tex") eqn
      _ -> do
        idxStr <- replaceAttr opts (Just label) (lookup "label" attrs) [] eqnRefs
        let eq' = eq++"\\qquad("++stringify idxStr++")"
        return $ Math DisplayMath eq'
replaceInlines opts (Math DisplayMath eq)
  | autoEqnLabels opts
  = case outFormat opts of
      f | isFormat "latex" f ->
        let eqn = "\\begin{equation}"++eq++"\\end{equation}"
        in return $ RawInline (Format "tex") eqn
      _ -> do
        idxStr <- replaceAttr opts Nothing Nothing [] eqnRefs
        let eq' = eq++"\\qquad("++stringify idxStr++")"
        return $ Math DisplayMath eq'
replaceInlines opts x@(Image attr@(label,cls,attrs) alt img@(src, tit))
  = do
    sf <- get subFig
    if | sf -> do
        let label' | "fig:" `isPrefixOf` label = label
                   | otherwise  = "fig:" ++ label
        idxStr <- replaceAttr opts (Just label') (lookup "label" attrs) alt imgRefs
        case outFormat opts of
          f | isFormat "latex" f ->
            return $ latexSubFigure x label
          _  ->
            let alt' = applyTemplate idxStr alt $ figureTemplate opts
                tit' | "nocaption" `elem` cls = fromMaybe tit $ stripPrefix "fig:" tit
                     | "fig:" `isPrefixOf` tit = tit
                     | otherwise = "fig:" ++ tit
            in return $ Image (label, cls, attrs) alt' (src, tit')
       | "fig:" `isPrefixOf` label && "fig:" `isPrefixOf` tit -> do
        idxStr <- replaceAttr opts (Just label) (lookup "label" attrs) alt imgRefs
        let alt' = case outFormat opts of
              f | isFormat "latex" f -> alt
              _  -> applyTemplate idxStr alt $ figureTemplate opts
        return $ Image attr alt' img
       | otherwise ->
        return x
replaceInlines _ x = return x

divBlocks :: Block -> Block
divBlocks (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = Div (label,[],[]) [Table (init title) align widths header cells]
divBlocks x = x

spanInlines :: [Inline] -> [Inline]
spanInlines (math@(Math DisplayMath _eq):ils)
  | c:ils' <- dropWhile (==Space) ils
  , Just label <- getRefLabel "eq" [c]
  = Span (label,[],[]) [math]:ils'
spanInlines x = x

getRefLabel :: String -> [Inline] -> Maybe String
getRefLabel _ [] = Nothing
getRefLabel tag ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , "}" `isSuffixOf` attr
  , ("{#"++tag++":") `isPrefixOf` attr
  = init `fmap` stripPrefix "{#" attr
getRefLabel _ _ = Nothing

replaceAttr :: Options -> Maybe String -> Maybe String -> [Inline] -> Accessor References RefMap -> WS [Inline]
replaceAttr o label refLabel title prop
  = do
    chap  <- take (chaptersDepth o) `fmap` get curChap
    i     <- (1+) `fmap` (M.size . M.filter (ap ((&&) . (chap ==) . init . refIndex) (isNothing . refSubfigure)) <$> get prop)
    let index = chap ++ [(i, refLabel <> customLabel o label' i)]
        label' = fromMaybe (show index) label
    hasLabel <- M.member label' <$> get prop
    when hasLabel $
      error $ "Duplicate label: " ++ label'
    modify prop $ M.insert label' RefRec {
      refIndex= index
    , refTitle=normalizeSpaces title
    , refSubfigure = Nothing
    }
    return $ chapPrefix (chapDelim o) index

replaceAttrSec :: String -> [Inline] -> Accessor References RefMap -> WS ()
replaceAttrSec label title prop
  = do
    index  <- get curChap
    modify prop $ M.insert label RefRec {
      refIndex=index
    , refTitle=normalizeSpaces title
    , refSubfigure = Nothing
    }
    return ()

latexSubFigure :: Inline -> String -> Inline
latexSubFigure (Image (_, cls, attrs) alt (src, title)) label =
  let
    title' = fromMaybe title $ stripPrefix "fig:" title
    texlabel | null label = []
             | otherwise = mkLaTeXLabel label
    texalt | "nocaption" `elem` cls  = []
           | otherwise =
              [ RawInline (Format "tex") "["] ++ alt ++ [ RawInline (Format "tex") "]"]
    img = Image (label, cls, attrs) alt (src, title')
  in Span stopAttr $
      [ RawInline (Format "tex") "\\subfloat" ] ++ texalt ++
      [ RawInline (Format "tex") "{" ] ++
      [img] ++
      [ RawInline (Format "tex") $ texlabel ++ "}"]
latexSubFigure x _ = x

stopAttr :: Attr
stopAttr = ([], ["crossref-stop"], [])
