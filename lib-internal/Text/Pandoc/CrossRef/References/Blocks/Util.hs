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

module Text.Pandoc.CrossRef.References.Blocks.Util where

import Control.Monad.Reader.Class
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walk)
import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Text.Read (readMaybe)

import Control.Applicative
import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..))

setLabel :: Options -> [Inline] -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
setLabel opts idx
  | setLabelAttribute opts
  = (("label", stringify idx) :)
  . filter ((/= "label") . fst)
  | otherwise = id

walkReplaceInlines :: [Inline] -> [Inline] -> Block -> Block
walkReplaceInlines newTitle title = walk replaceInlines
  where
  replaceInlines xs
    | xs == title = newTitle
    | otherwise = xs

-- | Exactly like 'Prefix' but doesn't have 'PfxSec'. @S@ stands for "safer".
-- Sections are handled specially, see
-- "Text.Pandoc.CrossRef.References.Blocks.Header"
data SPrefix
  = SPfxImg
  | SPfxEqn
  | SPfxTbl
  | SPfxLst

toPrefix :: SPrefix -> Prefix
toPrefix = \case
  SPfxImg -> PfxImg
  SPfxEqn -> PfxEqn
  SPfxTbl -> PfxTbl
  SPfxLst -> PfxLst

replaceAttr
  :: Maybe T.Text -- ^ Reference id
  -> [(T.Text, T.Text)] -- ^ Attributes
  -> [Inline] -- ^ Title
  -> SPrefix -- ^ Prefix type
  -> WS RefRec
replaceAttr label attrs title (toPrefix -> pfx) = do
  let refLabel = lookup "label" attrs
      number = readMaybe . T.unpack =<< lookup "number" attrs
  Options{..} <- ask
  chap  <- S.take chaptersDepth <$> use (ctrsAt PfxSec)
  prop' <- use $ refsAt pfx
  curIdx <- use $ ctrsAt pfx
  let i | Just n <- number = n
        | chap' :> last' <- S.viewr curIdx
        , chap' == chap
        = succ . fst $ last'
        | otherwise = 1
      index = chap S.|> (i, refLabel <|> customLabel (Pfx pfx) i)
      label' = fromMaybe (T.pack (show pfx <> (':' : show index))) label
  when (M.member label' prop') $
    error . T.unpack $ "Duplicate label: " <> label'
  globCtr <- stGlob <<%= (+ 1)
  ctrsAt pfx .= index
  refHideFromList <- checkHidden attrs
  let rec' = RefRec {
      refIndex= index
    , refGlobal = globCtr
    , refTitle= title
    , refSubfigure = Nothing
    , refHideFromList
    , refLabel = label
    }
  refsAt pfx %= M.insert label' rec'
  pure rec'

chapIndex :: RefRec -> WS [Inline]
chapIndex r = do
  Options{chapDelim} <- ask
  pure $ chapPrefix chapDelim $ refIndex r

mkCaption :: Options -> T.Text -> [Inline] -> Block
mkCaption opts style
  | outFormat opts == Just (Format "docx") = Div ("", [], [("custom-style", style)]) . return . Para
  | otherwise = Para

latexLabel :: RefRec -> [Inline]
latexLabel RefRec{refLabel}
  | Just lab <- refLabel = [ RawInline (Format "latex") $ mkLaTeXLabel lab ]
  | otherwise = mempty

latexCaption :: RefRec -> [Inline]
latexCaption ref = latexCaptionGeneric lcsRich ref <> latexLabel ref
  where
    lcsRich = LatexCaptionSpec
      { lcsEmptyShort = \caption ->
          [ RawInline (Format "latex") "\\caption[]", Span nullAttr caption ]
      , lcsWithoutShort = \caption ->
          [ RawInline (Format "latex") "\\caption", Span nullAttr caption ]
      , lcsWithShort = \short long ->
          [ RawInline (Format "latex") "\\caption["
          , Span nullAttr short
          , RawInline (Format "latex") "]"
          , Span nullAttr long ]
      }

data LatexCaptionSpec a = LatexCaptionSpec
  { lcsEmptyShort :: [Inline] -> a
  , lcsWithShort :: [Inline] -> [Inline] -> a
  , lcsWithoutShort :: [Inline] -> a
  }

-- | Only used in lstlistings code blocks.
latexCaptionRaw :: RefRec -> T.Text
latexCaptionRaw = latexCaptionGeneric lcsRaw
  where
    lcsRaw = LatexCaptionSpec
      { lcsEmptyShort = \caption -> "[]" <> toTeX caption
      , lcsWithShort = \short long -> "[" <> toTeX short <> "]" <> toTeX long
      , lcsWithoutShort = toTeX
      }
    toTeX = escapeLaTeX . stringify

latexCaptionGeneric :: Monoid a => LatexCaptionSpec a -> RefRec -> a
latexCaptionGeneric LatexCaptionSpec{..} ref
  | refHideFromList ref = lcsEmptyShort caption
  | refTitle ref == sansFootnotes = lcsWithoutShort caption
  | otherwise = lcsWithShort sansFootnotes caption
  where
    caption = refTitle ref
    sansFootnotes = removeFootnotes caption
    removeFootnotes = walk \case
      Note{} -> Str ""
      x -> x

checkHidden :: [(T.Text, T.Text)] -> WS Bool
checkHidden attrs = do
  hiddenHdr <- use stHiddenHeaderLevel
  pure . fromMaybe (isHdrHidden hiddenHdr) $
    not . isFalseValue <$> lookup "hidden" attrs

isFalseValue :: T.Text -> Bool
isFalseValue = (`Set.member` falseValues) . T.strip . T.toLower

falseValues :: Set T.Text
falseValues = Set.fromList
  [ "no"
  , "n"
  , "false"
  , "f"
  , "off"
  , "0"
  , "none"
  , "null"
  , "disable"
  , "disabled"
  , "unset"
  ]
