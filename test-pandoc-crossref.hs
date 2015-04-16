import Test.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Walk
import Util.Options
import Control.Monad.State
import References.Types
import Util.Settings
import qualified References.Blocks
import qualified Util.Template
import qualified Data.Map as M
import Data.Monoid
import Data.List (intersperse)

main :: IO ()
main = hspec $ do
    describe "References.Blocks.replaceBlocks" $ do
      it "Labels images" $
        testBlocks (figure "test.jpg" [] "Test figure" "figure")
        (figure "test.jpg" "fig:" "Figure 1: Test figure" [],
          def{imgRefs=M.fromList [("fig:figure",RefRec{refIndex=(0,1),refTitle=[Str "Test",Space,Str "figure"]})]})
      it "Labels equations" $
        testBlocks (equation "a^2+b^2=c^2" "equation")
        (equation "a^2+b^2=c^2\\qquad(1)" [],
          def{eqnRefs=M.fromList [("eq:equation",RefRec{refIndex=(0,1),refTitle=[]})]})
      it "Labels tables" $
        testBlocks (table' "Test table" "table")
        (table' "Table 1: Test table" [],
          def{tblRefs=M.fromList [("tbl:table",RefRec{refIndex=(0,1),refTitle=[Str "Test",Space,Str "table"]})]})

    describe "Util.Template" $
      it "Applies templates" $
        let template=Util.Template.makeTemplate defaultMeta (toList $ displayMath "figureTitle" <> displayMath "i" <> displayMath "t")
        in Util.Template.applyTemplate [Str "1"] [Str "title"] template `shouldBe`
           toList (str "Figure" <> str "1" <> str "title")

testBlocks :: Blocks -> (Blocks, References) -> Expectation
testBlocks arg res = runState (walkM (f defaultOptions) arg) def `shouldBe` res
  where f = References.Blocks.replaceBlocks

figure :: String -> String -> String -> String -> Blocks
figure src title alt ref = para (image src title (str' alt) <> ref' "fig" ref)

equation :: String -> String -> Blocks
equation eq ref = para (displayMath eq <> ref' "eq" ref)

table' :: String -> String -> Blocks
table' title ref = table (str' title <> ref' "tbl" ref) []
   [para $ str "H1", para $ str "H2"]
  [[para $ str "C1", para $ str "C2"]]

ref' :: String -> String -> Inlines
ref' p n | null n  = mempty
         | otherwise = space <> str ("{#"++p++":"++n++"}")

str' :: String -> Inlines
str' s = fromList $ intersperse Space $ Str `map` words s

defaultOptions :: Options
defaultOptions = getOptions defaultMeta Nothing
