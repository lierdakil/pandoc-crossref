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

{-# LANGUAGE CPP, OverloadedStrings #-}
import Test.Hspec
import Text.Pandoc hiding (runIO)
import qualified Text.Pandoc as P (runIO)
import Text.Pandoc.CrossRef
import System.FilePath
import System.Directory
import Control.Monad
import Text.Pandoc.Highlighting
import qualified Data.Text as T

listingsDirs :: [String]
listingsDirs = ["listings-code-block-caption-278"]

m2m :: String -> Spec
m2m dir
  | dir == "." = return ()
  | dir == ".." = return ()
  | otherwise =
  describe dir $ do
    input <- runIO $ readFile ("test" </> "m2m" </> dir </> "input.md")
    expect_md <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.md")
    let ro = def { readerExtensions = pandocExtensions }
        wo = def { writerExtensions = disableExtension Ext_raw_html $ disableExtension Ext_raw_attribute pandocExtensions
                 , writerHighlightStyle=Just pygments
                 , writerListings = dir `elem` listingsDirs }
    p@(Pandoc meta _) <- runIO $ either (error . show) id <$> P.runIO (readMarkdown ro $ T.pack input)
    let actual_md = either (fail . show) T.unpack $ runPure $ writeMarkdown wo $ runCrossRef meta (Just $ Format "markdown") defaultCrossRefAction p
    it "Markdown" $ do
      zipWithM_ shouldBe (lines' actual_md) (lines' expect_md)
      length' (lines' actual_md) `shouldBe` length' (lines' expect_md)
#ifdef FLAKY
    expect_tex <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.tex")
    let actual_tex = either (fail . show) T.unpack $ runPure $ writeLaTeX wo $ runCrossRef meta (Just $ Format "latex") defaultCrossRefAction p
    it "LaTeX" $ do
      zipWithM_ shouldBe (lines' actual_tex) (lines' expect_tex)
      length' (lines' actual_tex) `shouldBe` length' (lines' expect_tex)
#endif
  where
    lines' = zip [(1 :: Int)..] . lines
    length' = length . filter (not . null . snd)

main :: IO ()
main = do
  mds <-
#ifndef FLAKY
    filter (`notElem` flaky) <$>
#endif
    getDirectoryContents ("test" </> "m2m")
  hspec $
    describe "Integrative tests" $
      mapM_ m2m mds

#ifndef FLAKY
flaky :: [String]
flaky = [ "equations-tables"
        , "equations-tables-auto"
        , "subfigures-grid"
        ]
#endif
