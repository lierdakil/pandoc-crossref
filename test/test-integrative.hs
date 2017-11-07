{-# LANGUAGE CPP #-}
import Test.Hspec
import Text.Pandoc hiding (runIO)
import qualified Text.Pandoc as P (runIO)
import Text.Pandoc.CrossRef
import System.FilePath
import System.Directory
import Control.Monad
import Text.Pandoc.Highlighting
import qualified Data.Text as T

m2m :: String -> Spec
m2m dir
  | dir == "." = return ()
  | dir == ".." = return ()
  | otherwise =
  describe dir $ do
    input <- runIO $ readFile ("test" </> "m2m" </> dir </> "input.md")
    expect_md <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.md")
    expect_tex <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.tex")
    let ro = def { readerExtensions = pandocExtensions }
        wo = def { writerExtensions = pandocExtensions, writerHighlightStyle=Just pygments }
    p@(Pandoc meta _) <- runIO $ either (error . show) id <$> P.runIO (readMarkdown ro $ T.pack input)
    let actual_md = either (fail . show) T.unpack $ runPure $ writeMarkdown wo $ runCrossRef meta (Just $ Format "markdown") defaultCrossRefAction p
        actual_tex = either (fail . show) T.unpack $ runPure $ writeLaTeX wo $ runCrossRef meta (Just $ Format "latex") defaultCrossRefAction p
    it "Markdown" $ do
      zipWithM_ shouldBe (lines' actual_md) (lines' expect_md)
      length' (lines' actual_md) `shouldBe` length' (lines' expect_md)
    it "LaTeX" $ do
      zipWithM_ shouldBe (lines' actual_tex) (lines' expect_tex)
      length' (lines' actual_tex) `shouldBe` length' (lines' expect_tex)
  where
    lines' = zip [(1 :: Int)..] . lines
    length' = length . filter (not . null . snd)

main :: IO ()
main = do
  mds <- getDirectoryContents ("test" </> "m2m")
  hspec $
    describe "Integrative tests" $
      mapM_ m2m mds
