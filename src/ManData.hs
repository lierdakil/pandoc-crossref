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

{-# LANGUAGE TemplateHaskellQuotes, OverloadedStrings #-}
module ManData where

import Control.DeepSeq
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH.Syntax
import System.IO
import qualified Text.Pandoc as P
import Text.Pandoc.Highlighting (pygments)

dataFile :: FilePath
dataFile = "docs/index.md"

readDataFile :: IO T.Text
readDataFile =
  withFile dataFile ReadMode $ \h -> do
    hSetEncoding h utf8
    cont <- T.replace "* TOC\n{:toc}\n" "" <$> T.hGetContents h
    return $!! cont

embedManual :: (P.Pandoc -> P.PandocPure T.Text) -> Q Exp
embedManual fmt = do
  qAddDependentFile dataFile
  d <- runIO readDataFile
  let pd = either (error . show) id $ P.runPure $ P.readMarkdown readerOpts d
  let txt = either (error . show) id $ P.runPure $ fmt pd
  strToExp $ T.unpack txt

readerOpts :: P.ReaderOptions
readerOpts = P.def{
    P.readerExtensions = P.enableExtension P.Ext_yaml_metadata_block P.githubMarkdownExtensions
  , P.readerStandalone = True
}

embedManualText :: Q Exp
embedManualText = embedManual $ P.writePlain P.def

embedManualHtml :: Q Exp
embedManualHtml = do
  tt <- fmap (either (error . show) id) . runIO . P.runIO
          $   P.compileDefaultTemplate "html5"
  embedManual $ P.writeHtml5String P.def{
    P.writerTemplate = Just tt
  , P.writerHighlightStyle = Just pygments
  , P.writerTOCDepth = 6
  , P.writerTableOfContents = True
  }

strToExp :: String -> Q Exp
strToExp s =
    return $ VarE 'fromString
      `AppE` LitE (StringL s)
