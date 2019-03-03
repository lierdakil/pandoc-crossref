module Text.Pandoc.CrossRef.References.Subfigures where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.List
import Data.Maybe
import Data.Monoid

import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Prefixes
import Prelude

makeSubfigures :: Options -> Block -> Block
makeSubfigures opts (Div (label,cls,attrs) contents)
  | Just pfx <- getRefPrefix opts label
  , Right pfxRec <- getPfx opts pfx
  , prefixSubcaptions pfxRec
  = Div (label, "subcaption":cls, attrs)
    $ if prefixSubcaptionsGrid pfxRec
      then toTable (init cont) ++ [last cont]
      else cont
  where cont = walk figImageParas contents -- modified contents
        figImageParas (Para cs)
          | all isImageOrSpace cs
          = Para $ map addFigureAttr cs
        figImageParas (Plain cs)
          | all isImageOrSpace cs
          = Plain $ map addFigureAttr cs
        figImageParas x = x
        isImageOrSpace Image{} = True
        isImageOrSpace x = isSpace x
        addFigureAttr (Image attr alt (src, tit))
          | not $ "fig:" `isPrefixOf` tit
          = Image attr alt (src, "fig:" <> tit)
        addFigureAttr x = x
makeSubfigures _ x = x

toTable :: [Block] -> [Block]
toTable blks = [Table [] align widths [] $ map blkToRow blks]
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

latexEnv :: String -> [Block] -> [Inline] -> String -> Block
latexEnv env contents caption label =
  Div (label, [], []) $
    [ RawBlock (Format "latex") $ "\\begin{"<>env<>"}\n\\centering" ]
    ++ contents ++
    [ Para [RawInline (Format "latex") "\\caption"
             , Span nullAttr caption]
    , RawBlock (Format "latex") $ mkLaTeXLabel label
    , RawBlock (Format "latex") $ "\\end{"<>env<>"}"]
