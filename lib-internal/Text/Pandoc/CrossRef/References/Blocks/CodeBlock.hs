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

{-# LANGUAGE Rank2Types, OverloadedStrings, FlexibleContexts #-}

module Text.Pandoc.CrossRef.References.Blocks.CodeBlock where

import Control.Monad.Reader.Class
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)
import qualified Text.Pandoc.Builder as B
import Data.Function ((&))

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Blocks.Util (mkCaption, replaceAttr, setLabel)
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

runCodeBlock :: Attr -> T.Text -> Either T.Text [Inline] -> WS (ReplacedResult Block)
runCodeBlock (label, classes, attrs) code eCaption = do
  opts <- ask
  case outFormat opts of
    f --if used with listings package,nothing should be done
      | isLatexFormat f, listings opts -> eCaption &
        either
          (const noReplaceNoRecurse)
          (\caption -> replaceNoRecurse $
            CodeBlock (label,classes,("caption",escapeLaTeX $ stringify caption):attrs) code)
      --if not using listings, however, wrap it in a codelisting environment
      | isLatexFormat f ->
        replaceNoRecurse $ Div nullAttr [
            RawBlock (Format "latex") "\\begin{codelisting}"
          , Plain [
              RawInline (Format "latex") "\\caption"
            , Span nullAttr $ either (pure . Str) id eCaption
            ]
          , CodeBlock (label, classes, attrs) code
          , RawBlock (Format "latex") "\\end{codelisting}"
          ]
    _ -> do
      let cap = either (B.toList . B.text) id eCaption
      idxStr <- replaceAttr (Right label) (lookup "label" attrs) cap lstRefs
      let caption' = applyTemplate idxStr cap $ listingTemplate opts
      replaceNoRecurse $ Div (label, "listing":classes, []) [
          mkCaption opts "Caption" caption'
        , CodeBlock ("", classes, filter ((/="caption") . fst) $ setLabel opts idxStr attrs) code
        ]
