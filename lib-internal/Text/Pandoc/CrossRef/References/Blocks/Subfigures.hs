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

module Text.Pandoc.CrossRef.References.Blocks.Subfigures where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Data.Default (def)
import Data.List
import Data.Maybe
import Lens.Micro
import Lens.Micro.Mtl
import Text.Pandoc.Shared (blocksToInlines)

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Generic

runSubfigures :: Attr -> [Block] -> [Inline] -> WS (ReplacedResult Block)
runSubfigures (label, cls, attrs) images caption = do
  opts <- use wsOptions
  ref <- replaceAttr (Just label) attrs caption SPfxImg
  idxStr <- chapIndex ref
  glob <- use $ wsReferences . stGlob
  hiddenHdr <- use $ wsReferences . stHiddenHeaderLevel
  let opts' = opts
          { figureTemplate = subfigureChildTemplate opts
          , customLabel = customLabel opts . Sub
          }
  (cont, st) <- embedWS (WState opts' (References def def glob hiddenHdr))
    $ runReplace (mkRR replaceSubfigs) images
  let collectedCaptions = B.toList $
          intercalate' (B.fromList $ ccsDelim opts)
        $ map (B.fromList . collectCaps . snd)
        $ sortOn (refIndex . snd)
        $ filter (not . null . refTitle . snd)
        $ M.toList
        $ st ^. wsReferences . refsAt PfxImg
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
  let mangledSubfigures = mangleSubfigure <$> st ^. wsReferences . refsAt PfxImg
      mangleSubfigure v = v{refIndex = refIndex ref, refSubfigure = Just $ refIndex v}
  wsReferences . refsAt PfxImg %= (<> mangledSubfigures)
  wsReferences . stGlob .= st ^. wsReferences . stGlob
  -- stHiddenHeaderLevel shouldn't have changed, but for future-proofing...
  wsReferences . stHiddenHeaderLevel .= st ^. wsReferences . stHiddenHeaderLevel
  replaceNoRecurse $ if isLatexFormat opts
    then Div nullAttr $
      [ RawBlock (Format "latex") "\\begin{pandoccrossrefsubfigures}" ]
      <> cont <>
      [ Para $ latexCaption ref
      , RawBlock (Format "latex") "\\end{pandoccrossrefsubfigures}"]
    else Figure (label, "subfigures":cls, setLabel opts idxStr attrs)
                (Caption Nothing [Para capt])
      $ toTable opts cont
  where
    toTable :: Options -> [Block] -> [Block]
    toTable opts blks
      | subfigGrid opts = [simpleTable align (map ColWidth widths') (map (fmap pure . blkToRow) blks)]
      | subfigColumns opts = mapMaybe figurize blks
      | otherwise = blks
      where
        figurize x
          | Para is <- x = case (mapMaybe getImg is) `zip` widths' of
              [] -> Nothing -- skip empty Paras
              [_] -> error "The impossible happened: single-element Para in \
                \subfigure which is not a figure"
              xs -> Just $ Div ("", ["columns"], []) $ map columnize xs
          | otherwise = Just x
        columnize ((id', cs, as, alt, tgt), width) =
          Div ("", ["column"], [("width", T.pack (show (width * 100.0)) <> "%")]) [
            implicitFigure (id', cs, filter ((/="width") . fst) as) alt tgt Figure
          ]
        widths' = widths blks
        align | b:_ <- blks = let ils = blocksToInlines [b] in replicate (length $ mapMaybe getWidth ils) AlignCenter
              | otherwise = error "Misformatted subfigures block"
        blkToRow :: Block -> [Block]
        blkToRow (Para inls) = zipWith inlToCell widths' $ mapMaybe getImg inls
        blkToRow x = [x]
        getImg (Image (id', cs, as) txt tgt) = Just (id', cs, as, txt, tgt)
        getImg _ = Nothing
        inlToCell w (id', cs, as, txt, tgt) = implicitFigure (id', cs, setW w as) txt tgt Figure
        setW w as = ("width", width):filter ((/="width") . fst) as
          where
            -- With docx, since pandoc 3.0, 100% is interpreted as "page width",
            -- even in table cells. Hence, this hack.
            width
              | isDocxFormat opts = T.pack (show $ w * 100) <> "%"
              | otherwise = "100%"

getWidth :: Inline -> Maybe Double
getWidth (Image (_id, _class, as) _ _)
  = Just $ maybe 0 percToDouble $ lookup "width" as
  where
    percToDouble :: T.Text -> Double
    percToDouble percs
      | Right (perc, "%") <- T.double percs
      = perc/100.0
      | otherwise = error "Only percent allowed in subfigure width!"
getWidth _ = Nothing

widths :: [Block] -> [Double]
widths (b:_) = let ils = blocksToInlines [b] in fixZeros $ mapMaybe getWidth ils
  where
    fixZeros :: [Double] -> [Double]
    fixZeros ws
      = let nz = length $ filter (== 0) ws
            rzw = (0.99 - sum ws) / fromIntegral nz
        in if nz>0
            then map (\x -> if x == 0 then rzw else x) ws
            else ws
widths _ = error "Misformatted subfigures block"

replaceSubfigs :: Block -> WS (ReplacedResult Block)
replaceSubfigs (Figure attr caption' content) = runFigure True attr caption' content
-- this triggers when implicit_figures is disabled
replaceSubfigs (Para [Image attr caption' tgt]) =
  implicitFigure attr caption' tgt $ runFigure True
replaceSubfigs blk@(Para images) =
  let go _ [] = pure []
      go (w:ws) (x@Image{}:xs) = liftA2 (:) (replaceSubfig w x) (go ws xs)
      go ws (x:xs) = ([x] :) <$> go ws xs
  in replaceNoRecurse . Para . concat =<< go (widths [blk]) images
replaceSubfigs _ = noReplaceRecurse

-- this is mostly copy-paste from pandoc markdown reader
implicitFigure :: Attr -> [Inline] -> (T.Text, T.Text) -> (Attr -> Caption -> [Block] -> a) -> a
implicitFigure (ident, classes, attribs) capt' (url, title) f =
  let alt = maybe capt B.text $ lookup "alt" attribs
      capt = B.fromList capt'
      attribs' = filter (liftA2 (&&) (/= "latex-placement") (/= "alt") . fst) attribs
      figattribs = filter ((=="latex-placement") . fst) attribs
      figattr = (ident, mempty, figattribs)
      caption = B.simpleCaption $ B.plain capt
      figbody = B.plain $ B.imageWith ("", classes, attribs') url title alt
  in f figattr caption (B.toList figbody)

replaceSubfigs' :: [(Double, Inline)] -> WS [Inline]
replaceSubfigs' = fmap concat . mapM (uncurry replaceSubfig)

replaceSubfig :: Double -> Inline -> WS [Inline]
replaceSubfig width x@(Image (label,cls,attrs) alt tgt) = do
  opts <- use wsOptions
  ref <- replaceAttr (normalizeLabel label) attrs alt SPfxImg
  idxStr <- chapIndex ref
  let alt' = applyTemplate idxStr alt $ figureTemplate opts
  pure $ if isLatexFormat opts
    then latexSubFigure width x ref
    else [Image (fromMaybe "" (refLabel ref), cls, setLabel opts idxStr attrs) alt' tgt]
replaceSubfig _ _ = pure []

latexSubFigure :: Double -> Inline -> RefRec -> [Inline]
latexSubFigure width (Image (_, cls, attrs) alt (src, title)) ref =
  let
    title' = fromMaybe title $ T.stripPrefix "fig:" title
    texalt | "nocaption" `elem` cls  = []
           | otherwise = concat
              [ [ RawInline (Format "latex") "\\caption{"]
              , alt
              , latexLabel ref
              , [ RawInline (Format "latex") "}"]
              ]
    filterWidth = filter $ (/= "width") . fst
    img = Image (fromMaybe "" (refLabel ref), cls, filterWidth attrs) alt (src, title')
  in concat [
      [ RawInline (Format "latex") ("\\begin{subfigure}{" <> T.pack (show width) <> "\\linewidth}") ]
      , [ img ]
      , texalt
      , [RawInline (Format "latex") "\\end{subfigure}%\n"]
      ]
latexSubFigure _ x _ = [x]

normalizeLabel :: T.Text -> Maybe T.Text
normalizeLabel label
  | T.null label = Nothing
  | otherwise  = Just label

simpleTable :: [Alignment] -> [ColWidth] -> [[[Block]]] -> Block
simpleTable align width bod = Table nullAttr noCaption (zip align width)
  noTableHead [mkBody bod] noTableFoot
  where
  mkBody xs = TableBody nullAttr (RowHeadColumns 0) [] (map mkRow xs)
  mkRow xs = Row nullAttr (map mkCell xs)
  mkCell = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1)
  noCaption = Caption Nothing mempty
  noTableHead = TableHead nullAttr []
  noTableFoot = TableFoot nullAttr []

runFigure :: Bool -> Attr -> Caption -> [Block] -> WS (ReplacedResult Block)
runFigure subFigure (label, cls, fattrs) (Caption short (btitle : rest)) content = do
  opts <- use wsOptions
  let label' = normalizeLabel label
  let title = blocksToInlines [btitle]
      (attrs, content') = case blocksToInlines content of
        [Image attr@(_, _, as) _ tgt] ->
            -- the second argument is a fix for
            -- https://github.com/jgm/pandoc/issues/9720
            (fattrs <> as, \capt -> [Plain [Image attr capt tgt]])
        _ -> (fattrs, const content)
  ref <- replaceAttr label' attrs title SPfxImg
  idxStr <- chapIndex ref
  let listHidden = refHideFromList ref
  let short' | listHidden = Just mempty
             | otherwise = short
  let title'
        | isLatexFormat opts = title
        | otherwise = applyTemplate idxStr title $ figureTemplate opts
      caption' = Caption short' (walkReplaceInlines title' title btitle:rest)
  replaceNoRecurse $
    if subFigure && isLatexFormat opts
    then Plain $ case blocksToInlines content of
      ctHead:_ -> latexSubFigure 1.0 ctHead ref
      _ -> error "The impossible happened: empty content in subfigures"
    else Figure (label,cls,setLabel opts idxStr fattrs) caption' (content' title')
runFigure _ _ _ _ = noReplaceRecurse
