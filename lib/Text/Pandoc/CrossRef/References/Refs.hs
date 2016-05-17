{-# LANGUAGE TupleSections #-}
module Text.Pandoc.CrossRef.References.Refs (replaceRefs) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (normalizeInlines, normalizeSpaces)
import Control.Monad.State hiding (get, modify)
import Data.List
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
replaceRefsLatex prefix opts cits =
  return $ p [texcit]
  where
    texcit =
      RawInline (Format "tex") $
      if cref opts then
        cref'++"{"++listLabels prefix "" "," "" cits++"}"
        else
          listLabels prefix "\\ref{" ", " "}" cits
    p | cref opts = id
      | otherwise = getRefPrefix opts prefix cap (length cits - 1)
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
replaceRefsOther prefix opts cits = do
  indices <- mapM (getRefIndex prefix opts) cits
  let
    indices' = groupBy ((==) `on` (fmap init . fst)) (sort indices)
    cap = maybe False isFirstUpper $ getLabelPrefix . citationId . head $ cits
  return $ normalizeInlines $ getRefPrefix opts prefix cap (length cits - 1) $ intercalate [Str ",", Space]  (makeIndices opts `map` indices')

getRefIndex :: String -> Options -> Citation -> WS (Maybe Index, [Inline])
getRefIndex prefix opts Citation{citationId=cid,citationSuffix=suf}
  = do
    ref <- M.lookup lab <$> get prop
    let sub = join $ refSubfigure <$> ref
        idx = refIndex <$> ref
        suf' | Just sub' <- sub =
                    suf
                ++  [Space, Str "("]
                ++  makeIndices opts [(Just sub',[])]
                ++  [Str ")"]
             | otherwise = suf
    return (idx ,suf')
  where
  prop = lookupUnsafe prefix accMap
  lab = prefix ++ getLabelWithoutPrefix cid

makeIndices :: Options -> [(Maybe Index, [Inline])] -> [Inline]
makeIndices _ s | any (isNothing . fst) s = [Strong [Str "??"]]
makeIndices o s = intercalate sep $ reverse $ map f $ foldl' f2 [] $ map (A.first fromJust) $ filter (isJust . fst) s
  where
  f2 :: [[(Index, [Inline])]] -> (Index, [Inline]) -> [[(Index, [Inline])]]
  f2 [] (i,suf) = [[(i,suf)]]
  f2 ([]:xs) (i,suf) = [(i,suf)]:xs
  f2 l@(x@((ix,sufp):_):xs) (i,suf)
    | not (null suf) || not (null sufp) = [(i,suf)]:l
    | ni-hx == 0 = l        -- remove duplicates
    | ni-hx == 1 = ((i,[]):x):xs -- group sequental
    | otherwise     = [(i,[])]:l    -- new group
    where
      hx = fst $ last ix
      ni = fst $ last i
  f []  = []                          -- drop empty lists
  f [w] = show' w                    -- single value
  f [w1,w2] = show' w2 ++ sep ++ show' w1 -- two values
  f (x:xs) = show' (last xs) ++ rangeDelim o ++ show' x -- shorten more than two values
  sep = [Str ",", Space]
  show' (i,suf) = chapPrefix (chapDelim o) i ++ suf
