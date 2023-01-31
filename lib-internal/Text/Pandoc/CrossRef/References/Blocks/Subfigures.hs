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

{-# LANGUAGE Rank2Types, OverloadedStrings, FlexibleContexts, LambdaCase #-}

module Text.Pandoc.CrossRef.References.Blocks.Subfigures where

import Control.Monad.Reader
import Control.Monad.State hiding (get, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Data.Default (def)
import Data.List
import Data.Maybe
import Text.Pandoc.Walk (walk)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Pandoc.Shared (blocksToInlines)

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util (setLabel, replaceAttr, walkReplaceInlines)
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

runSubfigures :: Attr -> [Block] -> [Inline] -> WS (ReplacedResult Block)
runSubfigures (label, cls, attrs) images caption = do
  opts <- ask
  idxStr <- replaceAttr (Right label) (lookup "label" attrs) caption imgRefs
  let (cont, st) = flip runState def $ flip runReaderT opts' $ runWS $ runReplace (mkRR replaceSubfigs `extRR` doFigure) $ images
      doFigure :: Block -> WS (ReplacedResult Block)
      doFigure (Figure attr caption' content) = runFigure True attr caption' content
      doFigure _ = noReplaceRecurse
      opts' = opts
          { figureTemplate = subfigureChildTemplate opts
          , customLabel = \r i -> customLabel opts ("sub"<>r) i
          }
      collectedCaptions = B.toList $
          intercalate' (B.fromList $ ccsDelim opts)
        $ map (B.fromList . collectCaps . snd)
        $ sortOn (refIndex . snd)
        $ filter (not . null . refTitle . snd)
        $ M.toList
        $ st^.imgRefs
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
  lastRef <- fromJust . M.lookup label <$> use imgRefs
  modifying imgRefs $ \old ->
      M.union
        old
        (M.map (\v -> v{refIndex = refIndex lastRef, refSubfigure = Just $ refIndex v})
        $ st^.imgRefs)
  case outFormat opts of
    f | isLatexFormat f ->
      replaceNoRecurse $ Div nullAttr $
        [ RawBlock (Format "latex") "\\begin{pandoccrossrefsubfigures}" ]
        <> cont <>
        [ Para [RawInline (Format "latex") "\\caption["
                  , Span nullAttr (removeFootnotes caption)
                  , RawInline (Format "latex") "]"
                  , Span nullAttr caption]
        , RawBlock (Format "latex") $ mkLaTeXLabel label
        , RawBlock (Format "latex") "\\end{pandoccrossrefsubfigures}"]
    _ -> replaceNoRecurse
      $ Figure (label, "subfigures":cls, setLabel opts idxStr attrs) (Caption Nothing [Para capt])
      $ toTable opts cont
  where
    removeFootnotes = walk removeFootnote
    removeFootnote Note{} = Str ""
    removeFootnote x = x
    toTable :: Options -> [Block] -> [Block]
    toTable opts blks
      | isLatexFormat $ outFormat opts = concatMap imagesToFigures blks
      | subfigGrid opts = [simpleTable align (map ColWidth widths) (map (fmap pure . blkToRow) blks)]
      | otherwise = blks
      where
        align | b:_ <- blks = let ils = blocksToInlines [b] in replicate (length $ mapMaybe getWidth ils) AlignCenter
              | otherwise = error "Misformatted subfigures block"
        widths | b:_ <- blks = let ils = blocksToInlines [b] in fixZeros $ mapMaybe getWidth ils
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
        percToDouble :: T.Text -> Double
        percToDouble percs
          | Right (perc, "%") <- T.double percs
          = perc/100.0
          | otherwise = error "Only percent allowed in subfigure width!"
        blkToRow :: Block -> [Block]
        blkToRow (Para inls) = mapMaybe inlToCell inls
        blkToRow x = [x]
        inlToCell :: Inline -> Maybe Block
        inlToCell (Image (id', cs, as) txt tgt) = Just $
          Figure (id', cs, []) (Caption Nothing [Para txt]) [Plain [Image ("", cs, setW as) txt tgt]]
        inlToCell _ = Nothing
        setW as = ("width", "100%"):filter ((/="width") . fst) as

replaceSubfigs :: [Inline] -> WS (ReplacedResult [Inline])
replaceSubfigs = (replaceNoRecurse . concat) <=< mapM replaceSubfig

imagesToFigures :: Block -> [Block]
imagesToFigures = \case
  x@Figure{} -> [x]
  Para xs -> mapMaybe imageToFigure xs
  Plain xs -> mapMaybe imageToFigure xs
  _ -> []

imageToFigure :: Inline -> Maybe Block
imageToFigure = \case
  Image (label,cls,attrs) alt tgt -> Just $
    Figure (label, cls, []) (Caption Nothing [Para alt])
      [Plain [Image ("",cls,attrs) alt tgt]]
  _ -> Nothing

replaceSubfig :: Inline -> WS [Inline]
replaceSubfig x@(Image (label,cls,attrs) alt tgt)
  = do
      opts <- ask
      let label' = normalizeLabel label
      idxStr <- replaceAttr label' (lookup "label" attrs) alt imgRefs
      let alt' = applyTemplate idxStr alt $ figureTemplate opts
      case outFormat opts of
        f | isLatexFormat f -> pure $ latexSubFigure x label
        _ -> pure [Image (label, cls, setLabel opts idxStr attrs) alt' tgt]
replaceSubfig x = pure [x]

latexSubFigure :: Inline -> T.Text -> [Inline]
latexSubFigure (Image (_, cls, attrs) alt (src, title)) label =
  let
    title' = fromMaybe title $ T.stripPrefix "fig:" title
    texlabel | T.null label = []
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

normalizeLabel :: T.Text -> Either T.Text T.Text
normalizeLabel label
  | "fig:" `T.isPrefixOf` label = Right label
  | T.null label = Left "fig"
  | otherwise  = Right $ "fig:" <> label

simpleTable :: [Alignment] -> [ColWidth] -> [[[Block]]] -> Block
simpleTable align width bod = Table nullAttr noCaption (zip align width)
  noTableHead [mkBody bod] noTableFoot
  where
  mkBody xs = TableBody nullAttr (RowHeadColumns 0) [] (map mkRow xs)
  mkRow xs = Row nullAttr (map mkCell xs)
  mkCell xs = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) xs
  noCaption = Caption Nothing mempty
  noTableHead = TableHead nullAttr []
  noTableFoot = TableFoot nullAttr []

runFigure :: Bool -> Attr -> Caption -> [Block] -> WS (ReplacedResult Block)
runFigure subFigure (label, cls, fattrs) (Caption short (btitle : rest)) content = do
  opts <- ask
  let label' = normalizeLabel label
  let title = blocksToInlines [btitle]
      attrs = case blocksToInlines content of
        [Image (_, _, as) _ _] -> fattrs <> as
        _ -> fattrs
  idxStr <- replaceAttr label' (lookup "label" attrs) title imgRefs
  let title' = case outFormat opts of
        f | isLatexFormat f -> title
        _  -> applyTemplate idxStr title $ figureTemplate opts
      caption' = Caption short (walkReplaceInlines title' title btitle:rest)
  replaceNoRecurse $
    if subFigure && isLatexFormat (outFormat opts)
    then Plain $ latexSubFigure (head $ blocksToInlines content) label
    else Figure (label,cls,setLabel opts idxStr fattrs) caption' content
runFigure _ _ _ _ = noReplaceNoRecurse
