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

module Text.Pandoc.CrossRef.References.Blocks.Table where

import Control.Monad.Reader.Class
import Text.Pandoc.Definition
import Text.Pandoc.Shared (blocksToInlines)
import Data.Function ((&))

import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

runTable :: Attr -> Maybe Attr -> Maybe ShortCaption -> Block -> [Block] -> [ColSpec] -> TableHead -> [TableBody] -> TableFoot -> WS (ReplacedResult Block)
runTable (label, clss, attrs) mtattr short btitle rest colspec header cells foot = do
  opts <- ask
  ref <- replaceAttr (Just label) attrs title SPfxTbl
  idxStr <- chapIndex ref
  let short' | refHideFromList ref = Just mempty
             | otherwise = short
      title'
        | isLatexFormat opts = latexLabel ref <> title
        | otherwise = applyTemplate idxStr title $ tableTemplate opts
      caption' = Caption short' (walkReplaceInlines title' title btitle:rest)
      label' | isLatexFormat opts = ""
             | otherwise = label
  replaceNoRecurse $ (mtattr &
    maybe
      (Table (label, clss, setLabel opts idxStr attrs))
      (\tattr a b c d -> Div (label', clss, setLabel opts idxStr attrs) . pure . Table tattr a b c d)
    )
    caption' colspec header cells foot
  where title = blocksToInlines [btitle]
