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

data Options = Options { cref :: Bool
                       , chaptersDepth   :: Int
                       , listings :: Bool
                       , codeBlockCaptions  :: Bool
                       , autoSectionLabels  :: Bool
                       , numberSections  :: Bool
                       , sectionsDepth  :: Int
                       -- , figPrefix   :: Bool -> Int -> [Inline]
                       -- , eqnPrefix   :: Bool -> Int -> [Inline]
                       -- , tblPrefix   :: Bool -> Int -> [Inline]
                       -- , lstPrefix   :: Bool -> Int -> [Inline]
                       -- , secPrefix   :: Bool -> Int -> [Inline]
                       -- , figPrefixTemplate :: Template
                       -- , eqnPrefixTemplate :: Template
                       -- , tblPrefixTemplate :: Template
                       -- , lstPrefixTemplate :: Template
                       -- , secPrefixTemplate :: Template
                       , refIndexTemplate :: Template
                       , subfigureRefIndexTemplate :: Template
                       , secHeaderTemplate :: Template
                       , chapDelim   :: Inlines
                       , rangeDelim  :: Inlines
                       , pairDelim  :: Inlines
                       , lastDelim  :: Inlines
                       , refDelim  :: Inlines
                       -- , lofTitle    :: [Block]
                       -- , lotTitle    :: [Block]
                       -- , lolTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       -- , figureTemplate :: Template
                       -- , subfigureTemplate :: Template
                       -- , subfigureChildTemplate :: Template
                       -- , ccsTemplate :: Template
                       -- , tableTemplate  :: Template
                       -- , listingTemplate :: Template
                       -- , customLabel :: String -> Int -> Maybe String
                       , ccsDelim :: Inlines
                       , ccsLabelSep :: Inlines
                       , tableEqns :: Bool
                       , autoEqnLabels :: Bool
                       , subfigGrid :: Bool
                       , linkReferences :: Bool
                       , nameInLink :: Bool
                       , prefixes :: Prefixes
                       -- TODO: Defaults for prefix settings
                       }

prefixList :: Options -> [String]
prefixList = M.keys . prefixes

pfxCaptionTemplate :: Options -> String -> Maybe Template
pfxCaptionTemplate opts pfx = prefixCaptionTemplate <$> M.lookup pfx (prefixes opts)
