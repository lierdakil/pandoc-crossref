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
    expect_md <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.md")
    expect_tex <- runIO $ readFile ("test" </> "m2m" </> dir </> "expect.tex")
    p@(Pandoc meta _) <- either (fail . show) return $ readMarkdown def input
    let actual_md = writeMarkdown def $ runCrossRef meta (Just $ Format "markdown") defaultCrossRefAction p
        actual_tex = writeLaTeX def $ runCrossRef meta (Just $ Format "latex") defaultCrossRefAction p
    it "Markdown" $ zipWithM_ shouldBe (lines' actual_md) (lines' expect_md)
    it "LaTeX" $ zipWithM_ shouldBe (lines' actual_tex) (lines' expect_tex)

main :: IO ()
main = do
  mds <- getDirectoryContents ("test" </> "m2m")
  hspec $
    describe "Integrative tests" $
      mapM_ m2m mds
