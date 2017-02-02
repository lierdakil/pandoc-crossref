{-# LANGUAGE CPP #-}
import Test.Hspec
import Text.Pandoc
import Text.Pandoc.CrossRef
import System.FilePath
import System.Directory
import Control.Monad

lines' :: String -> [(Integer, String)]
lines' = zip [1..] . lines

-- m2m :: Meta -> String -> String -> Expectation
m2m :: String -> Spec
m2m dir
  | dir == "." = return ()
  | dir == ".." = return ()
  | otherwise =
  describe dir $ do
    input <- runIO $ readFile ("test" </> "m2m" </> dir </> "input.md")
#if MIN_VERSION_pandoc(1,19,0)
    expect_md <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect-1.19.md")
#else
    expect_md <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.md")
#endif
    expect_tex <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.tex")
    p@(Pandoc meta _) <- either (fail . show) return $ readMarkdown def input
    let actual_md = writeMarkdown def $ runCrossRef meta (Just $ Format "markdown") defaultCrossRefAction p
        actual_tex = writeLaTeX def $ runCrossRef meta (Just $ Format "latex") defaultCrossRefAction p
    it "Markdown" $ do
      zipWithM_ shouldBe (lines' actual_md) (lines' expect_md)
      length (lines' actual_md) `shouldBe` length (lines' expect_md)
    it "LaTeX" $ do
      zipWithM_ shouldBe (lines' actual_tex) (lines' expect_tex)
      length (lines' actual_tex) `shouldBe` length (lines' expect_tex)

main :: IO ()
main = do
  mds <- getDirectoryContents ("test" </> "m2m")
  hspec $
    describe "Integrative tests" $
      mapM_ m2m mds
