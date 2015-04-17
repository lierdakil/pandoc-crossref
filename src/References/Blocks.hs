module References.Blocks (replaceBlocks) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify, normalizeSpaces)
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import Util.Accessor
import References.Types
import References.Accessors
import Util.Util
import Util.Options
import Util.Template

replaceBlocks :: Options -> Block -> WS Block
replaceBlocks opts x@(Header 1 _ _)
  | sepChapters opts
  = do
    modify (\r@References{curChap=cc} -> r{curChap=cc+1})
    return x
replaceBlocks opts (Para (Image alt img:c))
  | Just label <- getRefLabel "fig" c
  = do
    idxStr <- replaceAttr opts label alt imgRefs'
    let alt' = case outFormat opts of
          Just f | isFormat "latex" f ->
            RawInline (Format "tex") ("\\label{"++label++"}") : alt
          _  -> applyTemplate idxStr alt $ figureTemplate opts
    return $ Para [Image alt' (fst img,"fig:")]
replaceBlocks opts (Para (Math DisplayMath eq:c))
  | Just label <- getRefLabel "eq" c
  = case outFormat opts of
      Just f | isFormat "latex" f ->
        let eqn = "\\begin{equation}"++eq++"\\label{"++label++"}\\end{equation}"
        in return $ Para [RawInline (Format "tex") eqn]
      _ -> do
        idxStr <- replaceAttr opts label [] eqnRefs'
        let eq' = eq++"\\qquad("++stringify idxStr++")"
        return $ Para [Math DisplayMath eq']
replaceBlocks opts (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = do
    idxStr <- replaceAttr opts label (init title) tblRefs'
    let title' =
          case outFormat opts of
              Just f | isFormat "latex" f ->
                RawInline (Format "tex") ("\\label{"++label++"}") : init title
              _  -> applyTemplate idxStr (init title) $ tableTemplate opts
    return $ Table title' align widths header cells
replaceBlocks _ x = return x

getRefLabel :: String -> [Inline] -> Maybe String
getRefLabel _ [] = Nothing
getRefLabel tag ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , "}" `isSuffixOf` attr
  , ("{#"++tag++":") `isPrefixOf` attr
  = init `fmap` stripPrefix "{#" attr
getRefLabel _ _ = Nothing

replaceAttr :: Options -> String -> [Inline] -> Accessor References RefMap -> WS [Inline]
replaceAttr o label title prop
  = do
    chap  <- gets curChap
    index <- (1+) `fmap` gets (M.size . getProp prop)
    modify $ modifyProp prop $ M.insert label RefRec {
      refIndex=(chap,index)
    , refTitle=normalizeSpaces title
    }
    if sepChapters o
    then return $ Str (show chap) : chapDelim o ++ [Str (show index)]
    else return [Str (show index)]
