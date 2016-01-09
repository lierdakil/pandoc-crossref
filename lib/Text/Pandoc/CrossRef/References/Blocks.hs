{-# LANGUAGE CPP #-}
module Text.Pandoc.CrossRef.References.Blocks
  ( divBlocks
  , replaceBlocks
  , spanInlines
  , replaceInlines
  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.Shared (stringify, normalizeSpaces)
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import Text.Pandoc.CrossRef.Util.Accessor
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Accessors
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template

replaceBlocks :: Options -> Block -> WS Block
replaceBlocks opts (Header n (label, cls, attrs) text')
  = do
    let label' = if autoSecLab opts && not ("sec:" `isPrefixOf` label)
                 then "sec:"++label
                 else label
    unless ("unnumbered" `elem` cls) $ do
      modify $ \r@References{curChap=cc} ->
        let ln = length cc
            cl = lookup "label" attrs
            inc l = init l ++ [(fst (last l) + 1, cl)]
            cc' | ln > n = inc $ take n cc
                | ln == n = inc cc
                | otherwise = cc ++ take (n-ln-1) (zip [1,1..] $ repeat Nothing) ++ [(1,cl)]
        in r{curChap=cc'}
      when ("sec:" `isPrefixOf` label') $ replaceAttrSec label' text' secRefs'
    return $ Header n (label', cls, attrs) text'
#if MIN_VERSION_pandoc(1,16,0)
#else
replaceBlocks opts (Div (label,_,attrs) [Plain [Image alt img]])
  | "fig:" `isPrefixOf` label
  = do
    idxStr <- replaceAttr opts label (lookup "label" attrs) alt imgRefs'
    let alt' = case outFormat opts of
          f | isFormat "latex" f ->
            RawInline (Format "tex") ("\\label{"++label++"}") : alt
          _  -> applyTemplate idxStr alt $ figureTemplate opts
    return $ Para [Image alt' img]
#endif
replaceBlocks opts (Div (label,_,attrs) [Table title align widths header cells])
  | not $ null title
  , "tbl:" `isPrefixOf` label
  = do
    idxStr <- replaceAttr opts label (lookup "label" attrs) title tblRefs'
    let title' =
          case outFormat opts of
              f | isFormat "latex" f ->
                RawInline (Format "tex") ("\\label{"++label++"}") : title
              _  -> applyTemplate idxStr title $ tableTemplate opts
    return $ Table title' align widths header cells
replaceBlocks opts cb@(CodeBlock (label, classes, attrs) code)
  | not $ null label
  , "lst:" `isPrefixOf` label
  , Just caption <- lookup "caption" attrs
  = case outFormat opts of
      f
        --if used with listings package,nothing shoud be done
        | isFormat "latex" f, useListings opts -> return cb
        --if not using listings, however, wrap it in a codelisting environment
        | isFormat "latex" f ->
          return $ Div nullAttr [
              RawBlock (Format "tex")
                $ "\\begin{codelisting}\n\\caption{"++caption++"}"
            , cb
            , RawBlock (Format "tex") "\\end{codelisting}"
            ]
      _ -> do
        let cap = toList $ text caption
        idxStr <- replaceAttr opts label (lookup "label" attrs) cap lstRefs'
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
        | isFormat "latex" f, useListings opts ->
          return $ CodeBlock (label,classes,("caption",stringify caption):attrs) code
        --if not using listings, however, wrap it in a codelisting environment
        | isFormat "latex" f ->
          return $ Div nullAttr [
              RawBlock (Format "tex") "\\begin{codelisting}"
            , Para [
                RawInline (Format "tex") "\\caption{"
              , Span nullAttr caption
              , RawInline (Format "tex") "}"
              ]
            , CodeBlock (label,classes,attrs) code
            , RawBlock (Format "tex") "\\end{codelisting}"
            ]
      _ -> do
        idxStr <- replaceAttr opts label (lookup "label" attrs) caption lstRefs'
        let caption' = applyTemplate idxStr caption $ listingTemplate opts
        return $ Div (label, "listing":classes, []) [
            Para caption'
          , CodeBlock ([], classes, attrs) code
          ]
replaceBlocks opts x = bottomUpM (replaceInlines opts) x

replaceInlines :: Options -> [Inline] -> WS [Inline]
replaceInlines opts (Span (label,_,attrs) [Math DisplayMath eq]:ils')
  | "eq:" `isPrefixOf` label
  = case outFormat opts of
      f | isFormat "latex" f ->
        let eqn = "\\begin{equation}"++eq++"\\label{"++label++"}\\end{equation}"
        in return $ RawInline (Format "tex") eqn : ils'
      _ -> do
        idxStr <- replaceAttr opts label (lookup "label" attrs) [] eqnRefs'
        let eq' = eq++"\\qquad("++stringify idxStr++")"
        return $ Math DisplayMath eq' : ils'
#if MIN_VERSION_pandoc(1,16,0)
replaceInlines opts (Image attr@(label,_,attrs) alt img:ils')
  | "fig:" `isPrefixOf` label, "fig:" `isPrefixOf` snd img
  = do
    idxStr <- replaceAttr opts label (lookup "label" attrs) alt imgRefs'
    let alt' = case outFormat opts of
          f | isFormat "latex" f ->
            RawInline (Format "tex") ("\\label{"++label++"}") : alt
          _  -> applyTemplate idxStr alt $ figureTemplate opts
    return $ Image attr alt' img : ils'
#else
#endif
replaceInlines _ x = return x

divBlocks :: Block -> Block
#if MIN_VERSION_pandoc(1,16,0)
#else
divBlocks (Para (Image alt (img, title):c))
  | Just label <- getRefLabel "fig" c
  = Div (label,[],[]) [Plain [Image alt (img, "fig:" ++ title)]]
#endif
divBlocks (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = Div (label,[],[]) [Table (init title) align widths header cells]
divBlocks x = bottomUp spanInlines x

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

replaceAttr :: Options -> String -> Maybe String -> [Inline] -> Accessor References RefMap -> WS [Inline]
replaceAttr o label refLabel title prop
  = do
    chap  <- take (chapDepth o) `fmap` gets curChap
    i     <- (1+) `fmap` gets (M.size . M.filter ((==chap) . init . refIndex) . getProp prop)
    let index = chap ++ [(i, refLabel)]
    modify $ modifyProp prop $ M.insert label RefRec {
      refIndex= index
    , refTitle=normalizeSpaces title
    }
    return $ chapPrefix (chapDelim o) index

replaceAttrSec :: String -> [Inline] -> Accessor References RefMap -> WS ()
replaceAttrSec label title prop
  = do
    index  <- gets curChap
    modify $ modifyProp prop $ M.insert label RefRec {
      refIndex=index
    , refTitle=normalizeSpaces title
    }
    return ()
