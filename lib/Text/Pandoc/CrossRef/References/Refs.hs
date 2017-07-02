{-# LANGUAGE TupleSections #-}
module Text.Pandoc.CrossRef.References.Refs (replaceRefs) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (normalizeInlines, normalizeSpaces)
import Control.Monad.State hiding (get, modify)
import Data.List
import qualified Data.List.HT as HT
import Data.Maybe
import Data.Function
import qualified Data.Map as M
import Control.Arrow as A

import Data.Accessor
import Data.Accessor.Monad.Trans.State
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Control.Applicative
import Debug.Trace
import Prelude

replaceRefs :: Options -> [Inline] -> WS [Inline]
replaceRefs opts (Cite cits _:xs)
  = (++ xs) `fmap` intercalate [Str ",", Space] `fmap`
    mapM replaceRefs' (groupBy eqPrefix cits)
  where
    eqPrefix a b = uncurry (==) $
      (fmap uncapitalizeFirst . getLabelPrefix . citationId) <***> (a,b)
    (<***>) = join (***)
    replaceRefs' cits'
      | Just prefix <- allCitsPrefix cits'
      = replaceRefs'' prefix opts cits'
      | otherwise = return [Cite cits' il']
        where
          il' =  normalizeInlines $
              [Str "["]
            ++intercalate [Str ";", Space] (map citationToInlines cits')
            ++[Str "]"]
          citationToInlines c = normalizeSpaces $
            citationPrefix c ++ [Space, Str $ "@"++citationId c] ++ citationSuffix c
    replaceRefs'' = case outFormat opts of
                    f | isFormat "latex" f -> replaceRefsLatex
                    _                      -> replaceRefsOther
replaceRefs _ x = return x

-- accessors to state variables
accMap :: M.Map String (Accessor References RefMap)
accMap = M.fromList [("fig:",imgRefs)
                    ,("eq:" ,eqnRefs)
                    ,("tbl:",tblRefs)
                    ,("lst:",lstRefs)
                    ,("sec:",secRefs)
                    ]

-- accessors to options
prefMap :: M.Map String (Options -> Bool -> Int -> [Inline], Options -> Template)
prefMap = M.fromList [("fig:",(figPrefix, figPrefixTemplate))
                     ,("eq:" ,(eqnPrefix, eqnPrefixTemplate))
                     ,("tbl:",(tblPrefix, tblPrefixTemplate))
                     ,("lst:",(lstPrefix, lstPrefixTemplate))
                     ,("sec:",(secPrefix, secPrefixTemplate))
                     ]

prefixes :: [String]
prefixes = M.keys accMap

getRefPrefix :: Options -> String -> Bool -> Int -> [Inline] -> [Inline]
getRefPrefix opts prefix capitalize num cit =
  applyTemplate' (M.fromDistinctAscList [("i", cit), ("p", refprefix)])
        $ reftempl opts
  where (refprefixf, reftempl) = lookupUnsafe prefix prefMap
        refprefix = refprefixf opts capitalize num


lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromJust .) . M.lookup

allCitsPrefix :: [Citation] -> Maybe String
allCitsPrefix cits = find isCitationPrefix prefixes
  where
  isCitationPrefix p =
    all (p `isPrefixOf`) $ map (uncapitalizeFirst . citationId) cits

replaceRefsLatex :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex prefix opts cits
  | cref opts
  = replaceRefsLatex' prefix opts cits
  | otherwise
  = normalizeInlines . intercalate [Str ",", Space] <$>
      mapM (replaceRefsLatex' prefix opts) (groupBy citationGroupPred cits)

replaceRefsLatex' :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex' prefix opts cits =
  return $ p [texcit]
  where
    texcit =
      RawInline (Format "tex") $
      if cref opts then
        cref'++"{"++listLabels prefix "" "," "" cits++"}"
        else
          listLabels prefix "\\ref{" ", " "}" cits
    suppressAuthor = all (==SuppressAuthor) $ map citationMode cits
    noPrefix = all null $ map citationPrefix cits
    p | cref opts = id
      | suppressAuthor
      = id
      | noPrefix
      = getRefPrefix opts prefix cap (length cits - 1)
      | otherwise = ((citationPrefix (head cits) ++ [Space]) ++)
    cap = maybe False isFirstUpper $ getLabelPrefix . citationId . head $ cits
    cref' | suppressAuthor = "\\labelcref"
          | cap = "\\Cref"
          | otherwise = "\\cref"

listLabels :: String -> String -> String -> String -> [Citation] -> String
listLabels prefix p sep s =
  intercalate sep . map ((p ++) . (++ s) . mkLaTeXLabel' . (prefix++) . getLabelWithoutPrefix . citationId)

getLabelWithoutPrefix :: String -> String
getLabelWithoutPrefix = drop 1 . dropWhile (/=':')

getLabelPrefix :: String -> Maybe String
getLabelPrefix lab
  | uncapitalizeFirst p `elem` prefixes = Just p
  | otherwise = Nothing
  where p = (++ ":") . takeWhile (/=':') $ lab

replaceRefsOther :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsOther prefix opts cits =
  normalizeInlines . intercalate [Str ",", Space] <$>
    mapM (replaceRefsOther' prefix opts) (groupBy citationGroupPred cits)

citationGroupPred :: Citation -> Citation -> Bool
citationGroupPred = (==) `on` liftM2 (,) citationPrefix citationMode

replaceRefsOther' :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsOther' prefix opts cits = do
  indices <- mapM (getRefIndex prefix opts) cits
  let
    cap = maybe False isFirstUpper $ getLabelPrefix . citationId . head $ cits
    writePrefix | all (==SuppressAuthor) $ map citationMode cits
                = id
                | all null $ map citationPrefix cits
                = cmap $ getRefPrefix opts prefix cap (length cits - 1)
                | otherwise
                = cmap ((citationPrefix (head cits) ++ [Space]) ++)
    cmap f [Link attr t w]
      | nameInLink opts = [Link attr (f t) w]
    cmap f x = f x
  return $ normalizeInlines $ writePrefix (makeIndices opts indices)

data RefData = RefData { rdLabel :: String
                       , rdIdx :: Maybe Index
                       , rdSubfig :: Maybe Index
                       , rdSuffix :: [Inline]
                       } deriving (Eq)

instance Ord RefData where
  (<=) = (<=) `on` rdIdx

getRefIndex :: String -> Options -> Citation -> WS RefData
getRefIndex prefix _opts Citation{citationId=cid,citationSuffix=suf}
  = do
    ref <- M.lookup lab <$> get prop
    let sub = refSubfigure <$> ref
        idx = refIndex <$> ref
    return RefData
      { rdLabel = lab
      , rdIdx = idx
      , rdSubfig = join sub
      , rdSuffix = suf
      }
  where
  prop = lookupUnsafe prefix accMap
  lab = prefix ++ getLabelWithoutPrefix cid

makeIndices :: Options -> [RefData] -> [Inline]
makeIndices o s = intercalate sep $ map f $ HT.groupBy g $ sort $ nub s
  where
  g :: RefData -> RefData -> Bool
  g a b = all (null . rdSuffix) [a, b] && (
            all (isNothing . rdSubfig) [a, b] &&
            fromMaybe False ((liftM2 follows `on` rdIdx) b a) ||
            rdIdx a == rdIdx b &&
            fromMaybe False ((liftM2 follows `on` rdSubfig) b a)
          )
  follows :: Index -> Index -> Bool
  follows a b
    | Just (ai, al) <- HT.viewR a
    , Just (bi, bl) <- HT.viewR b
    = ai == bi && A.first (+1) bl == al
  follows _ _ = False
  f :: [RefData] -> [Inline]
  f []  = []                          -- drop empty lists
  f [w] = show' w                    -- single value
  f [w1,w2] = show' w1 ++ sep ++ show' w2 -- two values
  f (x:xs) = show' x ++ rangeDelim o ++ show' (last xs) -- shorten more than two values
  sep = [Str ",", Space]
  show' :: RefData -> [Inline]
  show' RefData{rdLabel=l, rdIdx=Just i, rdSubfig = sub, rdSuffix = suf}
    | linkReferences o = [Link nullAttr txt ('#':l,[])]
    | otherwise = txt
    where
      txt
        | Just sub' <- sub
        = let vars = M.fromDistinctAscList
                      [ ("i", chapPrefix (chapDelim o) i)
                      , ("s", chapPrefix (chapDelim o) sub')
                      , ("suf", suf)
                      ]
          in applyTemplate' vars $ subfigureRefIndexTemplate o
        | otherwise
        = let vars = M.fromDistinctAscList
                      [ ("i", chapPrefix (chapDelim o) i)
                      , ("suf", suf)
                      ]
          in applyTemplate' vars $ refIndexTemplate o
  show' RefData{rdLabel=l, rdIdx=Nothing, rdSuffix = suf} =
    trace ("Undefined cross-reference: " ++ l)
          (Strong [Str $ "¿" ++ l ++ "?"] : suf)
