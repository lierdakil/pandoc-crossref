import Test.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Walk
import Text.Pandoc.Generic
import Control.Monad.State
import Data.List
import Control.Arrow
import Data.Monoid -- needed for ghc<7.10
import qualified Data.Map as M

import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Settings
import Text.Pandoc.CrossRef.Util.Accessor
import Text.Pandoc.CrossRef.References.Accessors
import qualified Text.Pandoc.CrossRef.References.Blocks as References.Blocks
import qualified Text.Pandoc.CrossRef.References.Refs as References.Refs
import qualified Text.Pandoc.CrossRef.References.List as References.List
import qualified Text.Pandoc.CrossRef.Util.Template as Util.Template
import qualified Text.Pandoc.CrossRef.Util.CodeBlockCaptions as Util.CodeBlockCaptions

main :: IO ()
main = hspec $ do
    describe "References.Blocks.replaceBlocks" $ do
      it "Labels images" $
        testBlocks (figure "test.jpg" [] "Test figure" "figure")
        (figure "test.jpg" "fig:" "Figure 1: Test figure" [],
          def{imgRefs=M.fromList $ refRec' "fig:figure" 1 "Test figure"})
      it "Labels equations" $
        testBlocks (equation "a^2+b^2=c^2" "equation")
        (equation "a^2+b^2=c^2\\qquad(1)" [],
          def{eqnRefs=M.fromList $ refRec'' "eq:equation" 1})
      it "Labels tables" $
        testBlocks (table' "Test table" "table")
        (table' "Table 1: Test table" [],
          def{tblRefs=M.fromList $ refRec' "tbl:table" 1 "Test table"})
      it "Labels code blocks" $
        testBlocks (codeBlock' "Test code block" "codeblock")
        (codeBlockDiv "Listing 1: Test code block" "codeblock",
          def{lstRefs=M.fromList $ refRec' "lst:codeblock" 1 "Test code block"})
      it "Labels code block divs" $
        testBlocks (codeBlockDiv "Test code block" "codeblock")
        (codeBlockDiv "Listing 1: Test code block" "codeblock",
          def{lstRefs=M.fromList $ refRec' "lst:codeblock" 1 "Test code block"})
      it "Labels sections divs" $
        testBlocks (section "Section Header" 1 "section")
        (section "Section Header" 1 "section",
          def{secRefs=M.fromList $ refRec' "sec:section" 1 "Section Header",
              curChap=[(1,Nothing)]})

    describe "References.Refs.replaceRefs" $ do
      it "References one image" $
        testRefs' "fig:" [1] [4] imgRefs' "fig.\160\&4"
      it "References multiple images" $
        testRefs' "fig:" [1..3] [4..6] imgRefs' "figs.\160\&4-6"
      it "References one equation" $
        testRefs' "eq:" [1] [4] eqnRefs' "eq.\160\&4"
      it "References multiple equations" $
        testRefs' "eq:" [1..3] [4..6] eqnRefs' "eqns.\160\&4-6"
      it "References one table" $
        testRefs' "tbl:" [1] [4] tblRefs' "tbl.\160\&4"
      it "References multiple tables" $
        testRefs' "tbl:" [1..3] [4..6] tblRefs' "tbls.\160\&4-6"
      it "References one listing" $
        testRefs' "lst:" [1] [4] lstRefs' "lst.\160\&4"
      it "References multiple listings" $
        testRefs' "lst:" [1..3] [4..6] lstRefs' "lsts.\160\&4-6"
      it "References one section" $
        testRefs' "sec:" [1] [4] secRefs' "sec.\160\&4"
      it "References multiple sections" $
        testRefs' "sec:" [1..3] [4..6] secRefs' "secs.\160\&4-6"
      it "Separates references to different chapter items by a comma" $
        testRefs'' "lst:" [1..6] (zip [1,1..] [4..6] ++ zip [2,2..] [7..9]) lstRefs' "lsts.\160\&1.4-1.6, 2.7-2.9"

    describe "References.Refs.replaceRefs capitalization" $ do
      it "References one image" $
        testRefs' "Fig:" [1] [4] imgRefs' "Fig.\160\&4"
      it "References multiple images" $
        testRefs' "Fig:" [1..3] [4..6] imgRefs' "Figs.\160\&4-6"
      it "References one equation" $
        testRefs' "Eq:" [1] [4] eqnRefs' "Eq.\160\&4"
      it "References multiple equations" $
        testRefs' "Eq:" [1..3] [4..6] eqnRefs' "Eqns.\160\&4-6"
      it "References one table" $
        testRefs' "Tbl:" [1] [4] tblRefs' "Tbl.\160\&4"
      it "References multiple tables" $
        testRefs' "Tbl:" [1..3] [4..6] tblRefs' "Tbls.\160\&4-6"
      it "References one listing" $
        testRefs' "Lst:" [1] [4] lstRefs' "Lst.\160\&4"
      it "References multiple listings" $
        testRefs' "Lst:" [1..3] [4..6] lstRefs' "Lsts.\160\&4-6"
      it "References one listing" $
        testRefs' "Sec:" [1] [4] secRefs' "Sec.\160\&4"
      it "References multiple listings" $
        testRefs' "Sec:" [1..3] [4..6] secRefs' "Secs.\160\&4-6"

    describe "References.List.listOf" $ do
      it "Generates list of tables" $
        testList (para $ rawInline "tex" "\\listoftables")
                 def{tblRefs=M.fromList $ refRec' "tbl:1" 4 "4" <> refRec' "tbl:2" 5 "5" <> refRec' "tbl:3" 6 "6"}
                 (header 1 (text "List of Tables") <> orderedList ((plain . str . show) `map` [4..6 :: Int]))
      it "Generates list of figures" $
        testList (para $ rawInline "tex" "\\listoffigures")
                 def{imgRefs=M.fromList $ refRec' "fig:1" 4 "4" <> refRec' "fig:2" 5 "5" <> refRec' "fig:3" 6 "6"}
                 (header 1 (text "List of Figures") <> orderedList ((plain . str . show) `map` [4..6 :: Int]))

    describe "Util.CodeBlockCaptions" $
      it "Transforms table-style codeBlock captions to codeblock divs" $ do
        let t x = testCBCaptions x (codeBlockDiv "Code Block" "cb")
        t (codeBlockForTable "cb" <> paraText ": Code Block")
        t (codeBlockForTable "cb" <> paraText "Listing: Code Block")
        t (paraText ": Code Block" <> codeBlockForTable "cb")
        t (paraText "Listing: Code Block" <> codeBlockForTable "cb")

    describe "Util.Template" $
      it "Applies templates" $
        let template=Util.Template.makeTemplate defaultMeta (toList $ displayMath "figureTitle" <> displayMath "i" <> displayMath "t")
        in Util.Template.applyTemplate [Str "1"] [Str "title"] template `shouldBe`
           toList (str "Figure" <> str "1" <> str "title")

    describe "Citation groups shouldn't be separated (#22 regression test)" $ do
      it "Should not separate citation groups" $ do
        let cits = para $ citeGen "" [1..3]
        testRefs cits def cits

      it "Should not separate citation groups with unknown prefix" $ do
        let cits = para $ citeGen "unk:" [1..3]
        testRefs cits def cits

      it "Should not separate citation groups with different unknown prefixes" $ do
        let cits = para $ cite (mconcat $ map (cit . uncurry (++) . second show) l) $ text $
              "[" ++ intercalate "; " (map (("@"++) . uncurry (++) . second show) l) ++ "]"
            l = zip ["unk1:", "unk2:"] [1,2::Int]
        testRefs cits def cits


citeGen :: String -> [Int] -> Inlines
citeGen p l = cite (mconcat $ map (cit . (p++) . show) l) $ text $
  "[" ++ intercalate "; " (map (("@"++) . (p++) . show) l) ++ "]"

refGen :: String -> [Int] -> [Int] -> M.Map String RefRec
refGen p l1 l2 = M.fromList $ mconcat $ zipWith refRec'' (((uncapitalizeFirst p++) . show) `map` l1) l2

refGen' :: String -> [Int] -> [(Int, Int)] -> M.Map String RefRec
refGen' p l1 l2 = M.fromList $ mconcat $ zipWith refRec''' (((uncapitalizeFirst p++) . show) `map` l1) l2

refRec' :: String -> Int -> String -> [(String, RefRec)]
refRec' ref i tit = [(ref, RefRec{refIndex=[(i,Nothing)],refTitle=toList $ text tit})]

refRec'' :: String -> Int -> [(String, RefRec)]
refRec'' ref i = refRec' ref i []

refRec''' :: String -> (Int, Int) -> [(String, RefRec)]
refRec''' ref (c,i) = [(ref, RefRec{refIndex=[(c,Nothing), (i,Nothing)],refTitle=toList $ text []})]

testRefs' :: String -> [Int] -> [Int] -> Accessor References (M.Map String RefRec) -> String -> Expectation
testRefs' p l1 l2 prop res = testRefs (para $ citeGen p l1) (setProp prop (refGen p l1 l2) def) (para $ text res)

testRefs'' :: String -> [Int] -> [(Int, Int)] -> Accessor References (M.Map String RefRec) -> String -> Expectation
testRefs'' p l1 l2 prop res = testRefs (para $ citeGen p l1) (setProp prop (refGen' p l1 l2) def) (para $ text res)

testBlocks :: Blocks -> (Blocks, References) -> Expectation
testBlocks arg res = runState (walkM (f defaultOptions) arg) def `shouldBe` res
  where f = (. References.Blocks.divBlocks) . References.Blocks.replaceBlocks

testRefs :: Blocks -> References -> Blocks -> Expectation
testRefs bs st res = runState (bottomUpM (References.Refs.replaceRefs defaultOptions) (toList bs)) st `shouldBe` (toList res,st)

testCBCaptions :: Blocks -> Blocks -> Expectation
testCBCaptions bs res = runState (bottomUpM (Util.CodeBlockCaptions.codeBlockCaptions defaultOptions{cbCaptions=True}) (toList bs)) def `shouldBe` (toList res,def)

testList :: Blocks -> References -> Blocks -> Expectation
testList bs st res = runState (bottomUpM (References.List.listOf defaultOptions) (toList bs)) st `shouldBe` (toList res,st)

figure :: String -> String -> String -> String -> Blocks
figure src title alt ref = para (image src title (text alt) <> ref' "fig" ref)

section :: String -> Int -> String -> Blocks
section text' level label = headerWith ("sec:" ++ label,[],[]) level (text text')

equation :: String -> String -> Blocks
equation eq ref = para (displayMath eq <> ref' "eq" ref)

table' :: String -> String -> Blocks
table' title ref = table (text title <> ref' "tbl" ref) []
   [para $ str "H1", para $ str "H2"]
  [[para $ str "C1", para $ str "C2"]]

codeBlock' :: String -> String -> Blocks
codeBlock' title ref = codeBlockWith
  ("lst:"++ref,["haskell"],[("caption",title)]) "main :: IO ()"

codeBlockForTable :: String -> Blocks
codeBlockForTable ref = codeBlockWith
     ("lst:"++ref,["haskell"],[]) "main :: IO ()"

paraText :: String -> Blocks
paraText s = para $ text s

codeBlockDiv :: String -> String -> Blocks
codeBlockDiv title ref = divWith ("lst:"++ref, ["listing","haskell"],[]) $
  para (text title) <>
  codeBlockWith
    ("",["haskell"],[]) "main :: IO ()"

ref' :: String -> String -> Inlines
ref' p n | null n  = mempty
         | otherwise = space <> str ("{#"++p++":"++n++"}")

defaultOptions :: Options
defaultOptions = getOptions defaultMeta Nothing

defCit :: Citation
defCit = Citation{citationId = ""
                 ,citationPrefix = []
                 ,citationSuffix = []
                 ,citationHash = 0
                 ,citationNoteNum = 0
                 ,citationMode = NormalCitation
                 }

cit :: String -> [Citation]
cit r = [defCit{citationId=r}]
