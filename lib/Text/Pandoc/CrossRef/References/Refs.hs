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

module Text.Pandoc.CrossRef.References.Refs (replaceRefs) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
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
  = toList . (<> fromList xs) . intercalate' (text ", ") . map fromList <$> mapM replaceRefs' (groupBy eqPrefix cits)
  where
    eqPrefix a b = uncurry (==) $
      (fmap uncapitalizeFirst . getLabelPrefix . citationId) <***> (a,b)
    (<***>) = join (***)
    replaceRefs' cits'
      | Just prefix <- allCitsPrefix cits'
      = replaceRefs'' prefix opts cits'
      | otherwise = return [Cite cits' il']
        where
          il' = toList $
              str "["
            <> intercalate' (text "; ") (map citationToInlines cits')
            <> str "]"
          citationToInlines c =
            fromList (citationPrefix c) <> text ("@" ++ citationId c)
              <> fromList (citationSuffix c)
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
  = toList . intercalate' (text ", ") . map fromList <$>
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
replaceRefsOther prefix opts cits = toList . intercalate' (text ", ") . map fromList <$>
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
                = cmap $ toList . ((fromList (citationPrefix (head cits)) <> space) <>) . fromList
    cmap f [Link attr t w]
      | nameInLink opts = [Link attr (f t) w]
    cmap f x = f x
  return $ writePrefix (makeIndices opts indices)

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

data RefItem = RefRange RefData RefData | RefSingle RefData

makeIndices :: Options -> [RefData] -> [Inline]
makeIndices o s = format $ concatMap f $ HT.groupBy g $ sort $ nub s
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
  f :: [RefData] -> [RefItem]
  f []  = []                          -- drop empty lists
  f [w] = [RefSingle w]                   -- single value
  f [w1,w2] = [RefSingle w1, RefSingle w2] -- two values
  f (x:xs) = [RefRange x (last xs)] -- shorten more than two values
  format :: [RefItem] -> [Inline]
  format [] = []
  format [x] = toList $ show'' x
  format [x, y] = toList $ show'' x <> fromList (pairDelim o) <> show'' y
  format xs = toList $ intercalate' (fromList $ refDelim o) init' <> fromList (lastDelim o) <> last'
    where initlast []     = error "emtpy list in initlast"
          initlast [y]    = ([], y)
          initlast (y:ys) = first (y:) $ initlast ys
          (init', last') = initlast $ map show'' xs
  show'' :: RefItem -> Inlines
  show'' (RefSingle x) = show' x
  show'' (RefRange x y) = show' x <> fromList (rangeDelim o) <> show' y
  show' :: RefData -> Inlines
  show' RefData{rdLabel=l, rdIdx=Just i, rdSubfig = sub, rdSuffix = suf}
    | linkReferences o = link ('#':l) "" (fromList txt)
    | otherwise = fromList txt
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
          (strong (text $ "¿" ++ l ++ "?") <> fromList suf)
