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
      mapM (replaceRefsLatex' prefix opts) (groupBy ((==) `on` citationPrefix) cits)

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
    p | cref opts = id
      | all null $ map citationPrefix cits
      = getRefPrefix opts prefix cap (length cits - 1)
      | otherwise = ((citationPrefix (head cits) ++ [Space]) ++)
    cap = maybe False isFirstUpper $ getLabelPrefix . citationId . head $ cits
    cref' | cap = "\\Cref"
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
    mapM (replaceRefsOther' prefix opts) (groupBy ((==) `on` citationPrefix) cits)

replaceRefsOther' :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsOther' prefix opts cits = do
  indices <- mapM (getRefIndex prefix opts) cits
  let
    cap = maybe False isFirstUpper $ getLabelPrefix . citationId . head $ cits
    writePrefix | all null $ map citationPrefix cits
                = getRefPrefix opts prefix cap (length cits - 1)
                | otherwise
                = ((citationPrefix (head cits) ++ [Space]) ++)
  return $ normalizeInlines $ writePrefix (makeIndices opts indices)

getRefIndex :: String -> Options -> Citation -> WS (Either String Index, [Inline])
getRefIndex prefix opts Citation{citationId=cid,citationSuffix=suf}
  = do
    ref <- maybe (Left lab) Right . M.lookup lab <$> get prop
    let sub = refSubfigure <$> ref
        idx = refIndex <$> ref
        suf' | Right (Just sub') <- sub =
                    suf
                ++  [Space, Str "("]
                ++  makeIndices opts [(Right sub',[])]
                ++  [Str ")"]
             | otherwise = suf
    return (idx ,suf')
  where
  prop = lookupUnsafe prefix accMap
  lab = prefix ++ getLabelWithoutPrefix cid

makeIndices :: Options -> [(Either String Index, [Inline])] -> [Inline]
makeIndices o s = intercalate sep $ map f $ HT.groupBy g $ sort $ nub s
  where
  g :: (Either String Index, [Inline]) -> (Either String Index, [Inline]) -> Bool
  g a b = all (null . snd) [a, b] &&
          either (const False) id ((liftM2 follows `on` fst) b a)
  follows :: Index -> Index -> Bool
  follows a b
    | Just (ai, al) <- HT.viewR a
    , Just (bi, bl) <- HT.viewR b
    = ai == bi && A.first (+1) bl == al
  follows _ _ = False
  f :: [(Either String Index, [Inline])] -> [Inline]
  f []  = []                          -- drop empty lists
  f [w] = show' w                    -- single value
  f [w1,w2] = show' w1 ++ sep ++ show' w2 -- two values
  f (x:xs) = show' x ++ rangeDelim o ++ show' (last xs) -- shorten more than two values
  sep = [Str ",", Space]
  show' :: (Either String Index, [Inline]) -> [Inline]
  show' (Right i,suf) = chapPrefix (chapDelim o) i ++ suf
  show' (Left l,suf) = Strong [Str $ "¿" ++ l ++ "?"] : suf
