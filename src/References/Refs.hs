module References.Refs (replaceRefs) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (normalizeInlines)
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M

import Util.Accessor
import References.Types
import References.Accessors
import Util.Util
import Util.Options

replaceRefs :: Options -> [Inline] -> WS [Inline]
replaceRefs opts (Cite cits _:xs)
  | Just prefix <- allCitsPrefix cits
  = (++ xs) `fmap` replaceRefs' prefix opts cits
  where
    replaceRefs' = case outFormat opts of
                    Just f | isFormat "latex" f -> replaceRefsLatex
                    _                           -> replaceRefsOther
replaceRefs _ x = return x

-- accessors to state variables
accMap :: M.Map String (Accessor References RefMap)
accMap = M.fromList [("fig:",imgRefs')
                    ,("eq:" ,eqnRefs')
                    ,("tbl:",tblRefs')
                    ]

-- accessors to options
prefMap :: M.Map String (Options -> [Inline])
prefMap = M.fromList [("fig:",figPrefix)
                     ,("eq:" ,eqnPrefix)
                     ,("tbl:",tblPrefix)
                     ]

prefixes :: [String]
prefixes = M.keys accMap

getRefPrefix :: Options -> String -> [Inline]
getRefPrefix opts prefix | null refprefix = []
                         | otherwise   = refprefix ++ [Space]
                         where refprefix = lookupUnsafe prefix prefMap opts

lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromMaybe undefined .) . M.lookup

allCitsPrefix :: [Citation] -> Maybe String
allCitsPrefix cits = foldl f Nothing prefixes
  where
  f x@(Just _) _ = x
  f _ p | all (isPrefixOf p . citationId) cits = Just p
  f _ _ = Nothing

replaceRefsLatex :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex prefix opts cits =
  return $ p ++ [texcit]
  where
    texcit =
      RawInline (Format "tex") $
      if useCleveref opts then
        " \\cref{"++listLabels prefix "" "" cits++"}"
        else
          listLabels prefix " \\ref{" "}" cits
    p | useCleveref opts = []
      | otherwise = getRefPrefix opts prefix

listLabels :: String -> String -> String -> [Citation] -> String
listLabels prefix p s = foldl' joinStr "" . mapMaybe (getLabel prefix)
  where
  joinStr acc i | null acc  = p++i++s
                | otherwise = acc++","++p++i++s

getLabel :: String -> Citation -> Maybe String
getLabel prefix Citation{citationId=cid}
  | prefix `isPrefixOf` cid = Just cid
  | otherwise = Nothing

replaceRefsOther :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsOther prefix opts cits = do
  indices <- mapM (getRefIndex prefix) cits
  let
    indices' = groupBy ((==) `on` fmap fst) (sort indices)
  return $ getRefPrefix opts prefix ++ normalizeInlines (concatMap (makeIndices opts) indices')

getRefIndex :: String -> Citation -> WS (Maybe (Int, Int))
getRefIndex prefix Citation{citationId=cid}
  | prefix `isPrefixOf` cid
  = gets (fmap refIndex . M.lookup cid . getProp prop)
  | otherwise = return Nothing
  where
  prop = lookupUnsafe prefix accMap

makeIndices :: Options -> [Maybe (Int,Int)] -> [Inline]
makeIndices _ s | any isNothing s = [Str "??"]
makeIndices o s = intercalate sep $ reverse $ map f $ foldl' f2 [] $ catMaybes s
  where
  f2 [] i = [[i]]
  f2 ([]:xs) i = [i]:xs
  f2 l@(x@((_,hx):_):xs) i@(_,ni)
    | ni-hx == 0 = l        -- remove duplicates
    | ni-hx == 1 = (i:x):xs -- group sequental
    | otherwise     = [i]:l    -- new group
  f []  = []                          -- drop empty lists
  f [w] = show' w                    -- single value
  f [w1,w2] = show' w2 ++ sep ++ show' w1 -- two values
  f (x:xs) = show' (last xs) ++ rangeDelim o ++ show' x -- shorten more than two values
  sep = [Str ", "]
  show' (c,n) = if sepChapters o && c>0
    then [Str $ show c] ++ chapDelim o ++ [Str $ show n]
    else [Str $ show n]
