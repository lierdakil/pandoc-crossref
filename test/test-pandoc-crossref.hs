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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, CPP, OverloadedStrings
           , FlexibleInstances, StandaloneDeriving #-}
import Test.Hspec
import Text.Pandoc hiding (getDataFileName, Template)
import Text.Pandoc.Builder
import Control.Monad.State
import Data.List
import Data.Maybe
import Control.Arrow
import qualified Data.Map as M

import Text.Pandoc.CrossRef
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Prefixes
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Template.Types
import Data.Accessor hiding ((=:))
import qualified Text.Pandoc.CrossRef.References.Blocks as References.Blocks
import qualified Text.Pandoc.CrossRef.References.Refs as References.Refs
import qualified Text.Pandoc.CrossRef.References.List as References.List
import qualified Text.Pandoc.CrossRef.Util.Template as Util.Template
import qualified Text.Pandoc.CrossRef.Util.CodeBlockCaptions as Util.CodeBlockCaptions

#ifdef FLAKY
import qualified Data.Text as T
import qualified Native
import Paths_pandoc_crossref
#endif

import Data.String
import Prelude

main :: IO ()
main = hspec $ do
    describe "References.Blocks.replaceInlines" $ do
      it "Labels equations" $
        testAll (plain $ equation' "a^2+b^2=c^2" "equation")
        (plain $ spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" []),
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels equations in the middle of text" $
        testAll (plain $
                text "This is an equation: "
             <> equation' "a^2+b^2=c^2" "equation"
             <> text " it should be labeled")
        (plain $
           text "This is an equation: "
        <> spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" [])
        <> text " it should be labeled",
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels equations in the beginning of text" $
        testAll (plain $
                equation' "a^2+b^2=c^2" "equation"
             <> text " it should be labeled")
        (plain $
           spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" [])
        <> text " it should be labeled",
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels equations in the end of text" $
        testAll (plain $
                text "This is an equation: "
             <> equation' "a^2+b^2=c^2" "equation")
        (plain $
           text "This is an equation: "
        <> spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" []),
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )

    -- TODO:
    -- describe "References.Blocks.spanInlines"
    -- describe "References.Blocks.divBlocks"

    describe "References.Blocks.replaceBlocks" $ do
      it "Labels images" $
        testAll (figure "test.jpg" [] "Test figure" "figure")
        (figure "test.jpg" [] "Figure\160\&1: Test figure" "figure",
          (referenceData =: M.fromList [refRec' "fig:figure" 1 "Test figure" "Figure 1: Test figure"]) .
          (pfxCounter =: M.singleton "fig" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      -- it "Labels subfigures" $
      --   testAll (
      --     divWith ("fig:subfigure",[],[]) (
      --       para (figure' "fig:" "test1.jpg" [] "Test figure 1" "figure1")
      --     <>para (figure' "fig:" "test2.jpg" [] "Test figure 2" "figure2")
      --     <>para (text "figure caption")
      --       ) <>
      --     divWith ("fig:subfigure2",[],[]) (
      --       para (figure' "fig:" "test21.jpg" [] "Test figure 21" "figure21")
      --     <>para (figure' "fig:" "test22.jpg" [] "Test figure 22" "figure22")
      --     <>para (text "figure caption 2")
      --       )
      --     )
      --   (
      --     divWith ("fig:subfigure",["subfigures"],[]) (
      --          para (figure' "fig:" "test1.jpg" [] "a" "figure1")
      --       <> para (figure' "fig:" "test2.jpg" [] "b" "figure2")
      --       <> para (text "Figure 1: figure caption. a — Test figure 1, b — Test figure 2")
      --       ) <>
      --     divWith ("fig:subfigure2",["subfigures"],[]) (
      --          para (figure' "fig:" "test21.jpg" [] "a" "figure21")
      --       <> para (figure' "fig:" "test22.jpg" [] "b" "figure22")
      --       <> para (text "Figure 2: figure caption 2. a — Test figure 21, b — Test figure 22")
      --       )
      --   , (referenceData =: M.fromList [("fig:figure1",RefRec {
      --                                       refIndex = [(1,"1")],
      --                                       refTitle = fromList [Str "Test",Space,Str "figure",Space,Str "1"],
      --                                       refScope = Nothing,
      --                                       refLabel = "fig:figure1",
      --                                       refSubfigure = Just [(1, "a")]}),
      --                               ("fig:figure2",RefRec {
      --                                       refIndex = [(1,"1")],
      --                                       refTitle = fromList [Str "Test",Space,Str "figure",Space,Str "2"],
      --                                       refScope = Nothing,
      --                                       refLabel = "fig:figure2",
      --                                       refSubfigure = Just [(2, "b")]}),
      --                               ("fig:subfigure",RefRec {
      --                                       refIndex = [(1,"1")],
      --                                       refTitle = fromList [Str "figure",Space,Str "caption"],
      --                                       refScope = Nothing,
      --                                       refLabel = "fig:subfigure",
      --                                       refSubfigure = Nothing}),
      --                               ("fig:figure21",RefRec {
      --                                       refIndex = [(2,"2")],
      --                                       refTitle = fromList [Str "Test",Space,Str "figure",Space,Str "21"],
      --                                       refScope = Nothing,
      --                                       refLabel = "fig:figure21",
      --                                       refSubfigure = Just [(1, "a")]}),
      --                               ("fig:figure22",RefRec {
      --                                       refIndex = [(2,"2")],
      --                                       refTitle = fromList [Str "Test",Space,Str "figure",Space,Str "22"],
      --                                       refScope = Nothing,
      --                                       refLabel = "fig:figure22",
      --                                       refSubfigure = Just [(2, "b")]}),
      --                               ("fig:subfigure2",RefRec {
      --                                       refIndex = [(2,"2")],
      --                                       refTitle = fromList [Str "figure",Space,Str "caption",Space,Str "2"],
      --                                       refScope = Nothing,
      --                                       refLabel = "fig:subfigure2",
      --                                       refSubfigure = Nothing})
      --                              ]
      --       ) .
      --       (pfxCounter =: M.singleton "fig" 2) .
      --       (curChap =: M.singleton "fig" "fig:subfigure2")
      --       )
      it "Labels equations" $
        testAll (equation "a^2+b^2=c^2" "equation")
        (para $ spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" []),
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels equations in the middle of text" $
        testAll (para $
                text "This is an equation: "
             <> equation' "a^2+b^2=c^2" "equation"
             <> text " it should be labeled")
        (para $
           text "This is an equation: "
        <> spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" [])
        <> text " it should be labeled",
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels equations in the beginning of text" $
        testAll (para $
                equation' "a^2+b^2=c^2" "equation"
             <> text " it should be labeled")
        (para $
           spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" [])
        <> text " it should be labeled",
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels equations in the end of text" $
        testAll (para $
                text "This is an equation: "
             <> equation' "a^2+b^2=c^2" "equation")
        (para $
           text "This is an equation: "
        <> spanWith ("eq:equation", [], []) (equation' "a^2+b^2=c^2\\qquad(1)" []),
          (referenceData =: M.fromList [refRec' "eq:equation" 1 (math "a^2+b^2=c^2") "1"]) .
          (pfxCounter =: M.singleton "eq" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels tables" $
        testAll (table' "Test table" "table")
        (divWith ("tbl:table", [], []) $ table' "Table\160\&1: Test table" [],
          (referenceData =: M.fromList [refRec' "tbl:table" 1 "Test table" "Table 1: Test table"]) .
          (pfxCounter =: M.singleton "tbl" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels code blocks" $
        testAll (codeBlock' "Test code block" "codeblock")
        (codeBlockDiv' "Listing\160\&1: Test code block" "codeblock",
          (referenceData =: M.fromList [refRec' "lst:codeblock" 1 "Test code block" "Listing\160\&1: Test code block"]) .
          (pfxCounter =: M.singleton "lst" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels code block divs" $
        testAll (codeBlockDiv "Test code block" "codeblock")
        (codeBlockDiv' "Listing\160\&1: Test code block" "codeblock",
          (referenceData =: M.fromList [refRec' "lst:codeblock" 1 "Test code block" "Listing\160\&1: Test code block"]) .
          (pfxCounter =: M.singleton "lst" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )
      it "Labels sections divs" $
        testAll (section "Section Header" 1 "section")
        (section "Section Header" 1 "section",
            (referenceData ^= M.fromList [refRec' "sec:section" 1 "Section Header" ""])
          . (pfxCounter =: M.singleton "sec" $ CounterRec {crIndex = 1, crIndexInScope = M.singleton Nothing 1})
          )

    describe "References.Refs.replaceRefs" $ do
      it "References one image" $
        testRefs' "fig:" [1] [4] referenceData "fig.\160\&4"
      it "References multiple images" $
        testRefs' "fig:" [1..3] [4..6] referenceData "figs.\160\&4-6"
      it "References one equation" $
        testRefs' "eq:" [1] [4] referenceData "eq.\160\&4"
      it "References multiple equations" $
        testRefs' "eq:" [1..3] [4..6] referenceData "eqns.\160\&4-6"
      it "References one table" $
        testRefs' "tbl:" [1] [4] referenceData "tbl.\160\&4"
      it "References multiple tables" $
        testRefs' "tbl:" [1..3] [4..6] referenceData "tbls.\160\&4-6"
      it "References one listing" $
        testRefs' "lst:" [1] [4] referenceData "lst.\160\&4"
      it "References multiple listings" $
        testRefs' "lst:" [1..3] [4..6] referenceData "lsts.\160\&4-6"
      it "References one section" $
        testRefs' "sec:" [1] [4] referenceData "sec.\160\&4"
      it "References multiple sections" $
        testRefs' "sec:" [1..3] [4..6] referenceData "secs.\160\&4-6"
      it "Separates references to different chapter items by a comma" $
        let p = "lst:"
            cites = citeGen p [1..6]
            chap1 = snd $ refRec' "sec:1" 1 "Section 1" "Section 1"
            chap2 = snd $ refRec' "sec:2" 2 "Section 2" "Section 2"
            refs1 = M.map (\r -> r{refScope = Just chap1}) $ refGen p [1..3] [4..6]
            refs2 = M.map (\r -> r{refScope = Just chap2}) $ refGen p [4..6] [7..9]
            res = "lsts.\160\&4-6, 7-9"
        in testRefs (para cites) (setVal referenceData (refs1 <> refs2) def) (para $ text res)

    describe "References.Refs.replaceRefs capitalization" $ do
      it "References one image" $
        testRefs' "Fig:" [1] [4] referenceData "Fig.\160\&4"
      it "References multiple images" $
        testRefs' "Fig:" [1..3] [4..6] referenceData "Figs.\160\&4-6"
      it "References one equation" $
        testRefs' "Eq:" [1] [4] referenceData "Eq.\160\&4"
      it "References multiple equations" $
        testRefs' "Eq:" [1..3] [4..6] referenceData "Eqns.\160\&4-6"
      it "References one table" $
        testRefs' "Tbl:" [1] [4] referenceData "Tbl.\160\&4"
      it "References multiple tables" $
        testRefs' "Tbl:" [1..3] [4..6] referenceData "Tbls.\160\&4-6"
      it "References one listing" $
        testRefs' "Lst:" [1] [4] referenceData "Lst.\160\&4"
      it "References multiple listings" $
        testRefs' "Lst:" [1..3] [4..6] referenceData "Lsts.\160\&4-6"
      it "References one listing" $
        testRefs' "Sec:" [1] [4] referenceData "Sec.\160\&4"
      it "References multiple listings" $
        testRefs' "Sec:" [1..3] [4..6] referenceData "Secs.\160\&4-6"

    describe "References.List.listOf" $ do
      it "Generates list of tables" $
        testList (rawBlock "latex" "\\listof{tbl}")
                 (referenceData =: M.fromList [let l = "tbl:" <> show i; n = i + 3; sn = str $ show n in refRec' l n sn ("Table " <> sn <> ": " <> sn) | i <- [1..3]])
                 (header 1 (text "List of Tables") <> divWith ("",["list"],[]) (mconcat $ map (\i -> let n = show i in para $ text (n <> ". " <> n) ) [4..6 :: Int]))
      it "Generates list of figures" $
        testList (rawBlock "latex" "\\listof{fig}")
                 (referenceData =: M.fromList [let l = "fig:" <> show i; n = i + 3; sn = str $ show n in refRec' l n sn ("Figure " <> sn <> ": " <> sn) | i <- [1..3]])
                 (header 1 (text "List of Figures") <> divWith ("",["list"],[]) (mconcat $ map (\i -> let n = show i in para $ text (n <> ". " <> n) ) [4..6 :: Int]))

    describe "Util.CodeBlockCaptions" $
      it "Transforms table-style codeBlock captions to codeblock divs" $ do
        let t x = testCBCaptions x (codeBlockDiv "Code Block" "cb")
        t (codeBlockForTable "cb" <> paraText ": Code Block")
        t (codeBlockForTable "cb" <> paraText "Listing: Code Block")
        t (paraText ": Code Block" <> codeBlockForTable "cb")
        t (paraText "Listing: Code Block" <> codeBlockForTable "cb")

    describe "Util.Template" $
      it "Applies templates" $
        let template=Util.Template.makeTemplate
              (displayMath "figureTitle" <> displayMath "i" <> displayMath "t")
            vf "i" = Just $ MetaInlines $ toList $ text "1"
            vf "t" = Just $ MetaInlines $ toList $ text "title"
            vf "figureTitle" = Just $ toMetaValue $ text "Figure"
            vf _ = Nothing
        in Util.Template.applyTemplate template vf `shouldBe`
           (str "Figure" <> str "1" <> str "title")

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

#ifdef FLAKY
    describe "Test files" $ do

      it "demo.md matches demo.native" $ do
        demomd <- readFile =<< getDataFileName "docs/demo/demo.md"
        Pandoc m b <- handleError $ runPure $ readMarkdown def {readerExtensions = pandocExtensions} $ T.pack demomd
        let (res, _warn) = runCrossRef (Settings m) Nothing $ crossRefBlocks b
        res `shouldBe` Right Native.demo
#endif

citeGen :: String -> [Int] -> Inlines
citeGen p l = cite (mconcat $ map (cit . (p++) . show) l) $ text $
  "[" ++ intercalate "; " (map (("@"++) . (p++) . show) l) ++ "]"

refGen :: String -> [Int] -> [Int] -> M.Map String RefRec
refGen p l1 l2 = M.fromList $ zipWith (\r i -> refRec' r i mempty mempty) (((uncapitalizeFirst p++) . show) `map` l1) l2

refRec' :: String -> Int -> Inlines -> Inlines -> (String, RefRec)
refRec' ref i tit cap =
  let pfx = takeWhile (/=':') ref
      pfxRec = fromJust $ M.lookup pfx defaultPrefixes
  in ( ref
     , RefRec
       { refIndex=i
       , refIxInl = str $ show i
       , refIxInlRaw = str $ show i
       , refCaption= cap
       , refTitle=tit
       , refScope=Nothing
       , refLevel=0
       , refPfx=pfx
       , refLabel=ref
       , refAttrs = const Nothing
       , refPfxRec = pfxRec
       , refCaptionPosition = Below
       }
     )

testRefs :: Blocks -> References -> Blocks -> Expectation
testRefs bs st rbs = testState (bottomUpM References.Refs.replaceRefs) st bs (rbs, id)

testRefs' :: String -> [Int] -> [Int] -> Accessor References (M.Map String RefRec) -> String -> Expectation
testRefs' p l1 l2 prop res = testRefs (para $ citeGen p l1) (setVal prop (refGen p l1 l2) def) (para $ text res)

testAll :: Many Block -> (Many Block, References -> References) -> Expectation
testAll = testState References.Blocks.replaceAll def

evalCrossRefM :: CrossRefM c -> c
evalCrossRefM = evalCrossRefRes . runCrossRef (defaultMeta mempty) Nothing . CrossRef

evalCrossRefRes :: (Either WSException c, b) -> c
evalCrossRefRes = either (error . show) id . fst

instance Show Prefix where
  show _ = "Prefix{}"
instance Show Template where
  show _ = "Template{}"
instance Show (String -> Maybe MetaValue) where
  show _ = "String -> Maybe MetaValue"
deriving instance Show RefRec
deriving instance Show CaptionPosition
deriving instance Show CounterRec
deriving instance Eq CounterRec
deriving instance Show References
deriving instance Eq References
deriving instance Eq WSException

testState :: (Eq a1, Show a1) => ([a] -> WS [a1]) -> References -> Many a -> (Many a1, References -> References) -> Expectation
testState f init' arg (r, s) = evalCrossRefM $
  (`shouldBe` (toList r, s init')) <$> runStateT (unWS . f $ toList arg) init'

testCBCaptions :: Blocks -> Blocks -> Expectation
testCBCaptions bs res = bottomUp (Util.CodeBlockCaptions.mkCodeBlockCaptions defaultOptions{Text.Pandoc.CrossRef.Util.Options.codeBlockCaptions=True}) (toList bs) `shouldBe` toList res

testList :: Blocks -> (References -> References) -> Blocks -> Expectation
testList bs st res = testState (bottomUpM References.List.listOf) (st def) bs (res, st)

figure :: String -> String -> String -> String -> Blocks
figure = (((para .) .) .) . figure' "fig:"

figure' :: String -> String -> String -> String -> String -> Inlines
figure' p src title alt ref = imageWith ("fig:" ++ ref, [], []) src (p ++ title) (text alt)

section :: String -> Int -> String -> Blocks
section text' level label = headerWith ("sec:" ++ label,[],[]) level (text text')

equation :: String -> String -> Blocks
equation = (para .) . equation'

equation' :: String -> String -> Inlines
equation' eq ref = displayMath eq <> ref' "eq" ref

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
codeBlockDiv title ref = divWith ("lst:"++ref, [], []) $
  codeBlockWith ("",["haskell"],[]) "main :: IO ()"
  <> para (text $ ": " <> title)

codeBlockDiv' :: String -> String -> Blocks
codeBlockDiv' title ref = divWith ("lst:"++ref, ["listing", "haskell"], []) $
  para (text title)
  <> codeBlockWith ("",["haskell"],[]) "main :: IO ()"

ref' :: String -> String -> Inlines
ref' p n | null n  = mempty
         | otherwise = space <> str ("{#"++p++":"++n++"}")

defaultOptions :: Options
defaultOptions = getOptions (defaultMeta mempty) Nothing

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

infixr 0 =:
(=:) :: Accessor r a -> a -> r -> r
a =: b = a ^= b

defaultPrefixes :: Prefixes
defaultPrefixes = getPrefixes Nothing "prefixes" (defaultMeta mempty)
