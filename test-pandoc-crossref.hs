import Test.Hspec
import Text.Pandoc
import System.Process
-- import Control.Exception (evaluate)

main :: IO ()
main = do
  native <- read `fmap` readFile "demo.native" :: IO [Block]
  parsed <- read `fmap` readProcess "cabal" [
                              "exec", "pandoc", "--"
                            , "-F", "pandoc-crossref.hs"
                            , "-i", "demo.md"
                            , "-t", "native"]
                            ""
  hspec $
    describe "pandoc-crossref.hs" $
      it "formats demo.md as demo.native" $
        parsed `shouldBe` native
