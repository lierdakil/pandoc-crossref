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

module Text.Pandoc.CrossRef.Util.Options where
import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Prefixes
import qualified Data.Map as M
import Text.Pandoc.Builder
import Data.Maybe
import Data.List
import Data.List.Extra
import Text.Pandoc.CrossRef.Util.LatexPrefixes

data Options = Options { cref :: Bool
                       , chaptersDepth   :: Int
                       , listings :: Bool
                       , codeBlockCaptions  :: Bool
                       , autoSectionLabels  :: Bool
                       , numberSections  :: Bool
                       , sectionsDepth  :: Int
                       , refIndexTemplate :: Template
                       , subfigureRefIndexTemplate :: Template
                       , secHeaderTemplate :: Template
                       , chapDelim   :: Inlines
                       , rangeDelim  :: Inlines
                       , pairDelim  :: Inlines
                       , lastDelim  :: Inlines
                       , refDelim  :: Inlines
                       , outFormat   :: Maybe Format
                       , ccsTemplate :: Template
                       , ccsDelim :: Inlines
                       , ccsLabelSep :: Inlines
                       , tableEqns :: Bool
                       , autoEqnLabels :: Bool
                       , subfigGrid :: Bool
                       , linkReferences :: Bool
                       , nameInLink :: Bool
                       , prefixes :: Prefixes
                       , latexPrefixes :: LatexPrefixes
                       }

prefixList :: Options -> [String]
prefixList = M.keys . prefixes

pfxCaptionTemplate :: Options -> String -> Template
pfxCaptionTemplate opts pfx = prefixCaptionTemplate $ getPfx opts pfx

getPfx :: Options -> String -> Prefix
getPfx o pn = fromMaybe defaultPfx $ M.lookup pn $ prefixes o
  where
    defaultPfx = error $ "Undefined prefix: \"" <> pn <> "\""

getRefPrefix :: Options -> String -> Maybe String
getRefPrefix opts label
  | ':' `notElem` label = Nothing
  | otherwise =
    let pfx = takeWhile (/=':') label
    in if pfx `elem` prefixList opts
       then Just pfx
       else Nothing

getRefLabel :: Options -> [Inline] -> Maybe String
getRefLabel _ [] = Nothing
getRefLabel opts ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , Just lbl <- stripPrefix "{#" attr >>= stripSuffix "}"
  , Just _ <- getRefPrefix opts lbl
  = Just lbl
getRefLabel _ _ = Nothing

getTitleForListOf :: Options -> String -> Blocks
getTitleForListOf opts = prefixListOfTitle . getPfx opts
