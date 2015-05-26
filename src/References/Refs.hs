module References.Refs (replaceRefs) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (normalizeInlines, normalizeSpaces)
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M
import Control.Arrow as A

import Util.Accessor
import References.Types
import References.Accessors
import Util.Util
import Util.Options

replaceRefs :: Options -> [Inline] -> WS [Inline]
replaceRefs opts (Cite cits _:xs)
  = (++ xs) `fmap` intercalate [Str ",", Space] `fmap`
    mapM replaceRefs' (groupBy eqPrefix cits)
  where
    eqPrefix a b = uncurry (==) $
      (uncapitalizeFirst . getLabelPrefix . citationId) <***> (a,b)
    (<***>) = join (***)
    replaceRefs' cits'
      | Just prefix <- allCitsPrefix cits'
      = replaceRefs'' prefix opts cits'
      | otherwise = return [Cite cits' il']
        where
          il' = [Str "["]
            ++intercalate [Str ";"] (map citationToInlines cits')
            ++[Str "]"]
          citationToInlines c = normalizeSpaces $
            citationPrefix c ++ [Space, Str $ "@"++citationId c] ++ citationSuffix c
    replaceRefs'' = case outFormat opts of
                    f | isFormat "latex" f -> replaceRefsLatex
                    _                      -> replaceRefsOther
replaceRefs _ x = return x

-- accessors to state variables
accMap :: M.Map String (Accessor References RefMap)
accMap = M.fromList [("fig:",imgRefs')
                    ,("eq:" ,eqnRefs')
                    ,("tbl:",tblRefs')
                    ,("lst:",lstRefs')
                    ]

-- accessors to options
prefMap :: M.Map String (Options -> Bool -> Int -> [Inline])
prefMap = M.fromList [("fig:",figPrefix)
                     ,("eq:" ,eqnPrefix)
                     ,("tbl:",tblPrefix)
                     ,("lst:",lstPrefix)
                     ]

prefixes :: [String]
prefixes = M.keys accMap

getRefPrefix :: Options -> String -> Bool -> Int -> [Inline]
getRefPrefix opts prefix capitalize num
  | null refprefix = []
  | otherwise   = refprefix ++ [Str "\160"]
  where refprefix = lookupUnsafe prefix prefMap opts capitalize num

lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromJust .) . M.lookup

allCitsPrefix :: [Citation] -> Maybe String
allCitsPrefix cits = find isCitationPrefix prefixes
  where
  isCitationPrefix p =
    all (p `isPrefixOf`) $ map (uncapitalizeFirst . citationId) cits

replaceRefsLatex :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex prefix opts cits =
  return $ p ++ [texcit]
  where
    texcit =
      RawInline (Format "tex") $
      if useCleveref opts then
        "\\cref{"++listLabels prefix "" "," "" cits++"}"
        else
          listLabels prefix "\\ref{" ", " "}" cits
    p | useCleveref opts = []
      | otherwise = getRefPrefix opts prefix cap (length cits - 1)
    cap = isFirstUpper $ getLabelPrefix . citationId . head $ cits

listLabels :: String -> String -> String -> String -> [Citation] -> String
listLabels prefix p sep s =
  intercalate sep . map ((p ++) . (++ s) . (prefix++) . getLabelWithoutPrefix . citationId)

getLabelWithoutPrefix :: String -> String
getLabelWithoutPrefix = drop 1 . dropWhile (/=':')

getLabelPrefix :: String -> String
getLabelPrefix = (++ ":") . takeWhile (/=':')

replaceRefsOther :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsOther prefix opts cits = do
  indices <- mapM (getRefIndex prefix) cits
  let
    indices' = groupBy ((==) `on` (fmap fst . fst)) (sort indices)
    cap = isFirstUpper $ getLabelPrefix . citationId . head $ cits
  return $ normalizeInlines $ getRefPrefix opts prefix cap (length cits - 1) ++ concatMap (makeIndices opts) indices'

getRefIndex :: String -> Citation -> WS (Maybe (Int, Int), [Inline])
getRefIndex prefix Citation{citationId=cid,citationSuffix=suf}
  = (\x -> (x,suf)) `fmap` gets (fmap refIndex . M.lookup lab . getProp prop)
  where
  prop = lookupUnsafe prefix accMap
  lab = prefix ++ getLabelWithoutPrefix cid

makeIndices :: Options -> [(Maybe (Int, Int), [Inline])] -> [Inline]
makeIndices _ s | any (isNothing . fst) s = [Strong [Str "??"]]
makeIndices o s = intercalate sep $ reverse $ map f $ foldl' f2 [] $ map (A.first fromJust) $ filter (isJust . fst) s
  where
  f2 [] (i,suf) = [[(i,suf)]]
  f2 ([]:xs) (i,suf) = [(i,suf)]:xs
  f2 l@(x@(((_,hx),sufp):_):xs) (i@(_,ni),suf)
    | not (null suf) || not (null sufp) = [(i,suf)]:l
    | ni-hx == 0 = l        -- remove duplicates
    | ni-hx == 1 = ((i,[]):x):xs -- group sequental
    | otherwise     = [(i,[])]:l    -- new group
  f []  = []                          -- drop empty lists
  f [w] = show' w                    -- single value
  f [w1,w2] = show' w2 ++ sep ++ show' w1 -- two values
  f (x:xs) = show' (last xs) ++ rangeDelim o ++ show' x -- shorten more than two values
  sep = [Str ", "]
  show' ((c,n),suf) = (if sepChapters o && c>0
                          then [Str $ show c] ++ chapDelim o ++ [Str $ show n]
                          else [Str $ show n])
                      ++ suf
