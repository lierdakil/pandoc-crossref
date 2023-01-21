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

{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.CrossRef.References.Refs (replaceRefs) where

import Control.Arrow as A
import Control.Monad.Reader
import Data.Function
import Data.List
import qualified Data.List.HT as HT
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Text.Pandoc.Builder

import Control.Applicative
import Debug.Trace
import Lens.Micro.Mtl
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

replaceRefs :: [Inline] -> WS [Inline]
replaceRefs (Cite cits _:xs) = do
  opts <- ask :: WS Options
  toList . (<> fromList xs) . intercalate' (text ", ") . map fromList <$> mapM (replaceRefs' opts) (groupBy eqPrefix cits)
  where
    eqPrefix a b = uncurry (==) $
      (fmap uncapitalizeFirst . getLabelPrefix . citationId) <***> (a,b)
    (<***>) = join (***)
    replaceRefs' :: Options -> [Citation] -> WS [Inline]
    replaceRefs' opts cits'
      | Just prefix <- allCitsPrefix cits'
      = replaceRefs'' opts prefix cits'
      | otherwise = return [Cite cits' il']
        where
          il' = toList $
              str "["
            <> intercalate' (text "; ") (map citationToInlines cits')
            <> str "]"
          citationToInlines c =
            fromList (citationPrefix c) <> text ("@" <> citationId c)
              <> fromList (citationSuffix c)
    replaceRefs'' :: Options -> T.Text -> [Citation] -> WS [Inline]
    replaceRefs'' opts = ($ opts) . flip $ case outFormat opts of
                    f | isLatexFormat f -> replaceRefsLatex
                    _                   -> replaceRefsOther
replaceRefs x = return x

-- accessors to state variables
accMap :: M.Map T.Text ((RefMap -> Const RefMap RefMap) -> References -> Const RefMap References)
accMap = M.fromList [("fig:",imgRefs)
                    ,("eq:" ,eqnRefs)
                    ,("tbl:",tblRefs)
                    ,("lst:",lstRefs)
                    ,("sec:",secRefs)
                    ]

-- accessors to options
prefMap :: M.Map T.Text (Options -> Bool -> Int -> [Inline], Options -> Template)
prefMap = M.fromList [("fig:",(figPrefix, figPrefixTemplate))
                     ,("eq:" ,(eqnPrefix, eqnPrefixTemplate))
                     ,("tbl:",(tblPrefix, tblPrefixTemplate))
                     ,("lst:",(lstPrefix, lstPrefixTemplate))
                     ,("sec:",(secPrefix, secPrefixTemplate))
                     ]

prefixes :: [T.Text]
prefixes = M.keys accMap

getRefPrefix :: Options -> T.Text -> Bool -> Int -> [Inline] -> [Inline]
getRefPrefix opts prefix capitalize num cit =
  applyTemplate' (M.fromDistinctAscList [("i", cit), ("p", refprefix)])
        $ reftempl opts
  where (refprefixf, reftempl) = lookupUnsafe prefix prefMap
        refprefix = refprefixf opts capitalize num


lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromJust .) . M.lookup

allCitsPrefix :: [Citation] -> Maybe T.Text
allCitsPrefix cits = find isCitationPrefix prefixes
  where
  isCitationPrefix p =
    all ((p `T.isPrefixOf`) . uncapitalizeFirst . citationId) cits

replaceRefsLatex :: T.Text -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex prefix opts cits
  | cref opts
  = replaceRefsLatex' prefix opts cits
  | otherwise
  = toList . intercalate' (text ", ") . map fromList <$>
      mapM (replaceRefsLatex' prefix opts) (groupBy citationGroupPred cits)

replaceRefsLatex' :: T.Text -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex' prefix opts cits =
  return $ p [texcit]
  where
    texcit =
      RawInline (Format "tex") $
      if cref opts then
        cref'<>"{"<>listLabels prefix "" "," "" cits<>"}"
        else
          listLabels prefix "\\ref{" ", " "}" cits
    suppressAuthor = all ((==SuppressAuthor) . citationMode) cits
    noPrefix = all (null . citationPrefix) cits
    p | cref opts = id
      | suppressAuthor
      = id
      | noPrefix
      = getRefPrefix opts prefix cap (length cits - 1)
      | otherwise = ((citationPrefix (head cits) <> [Space]) <>)
    cap = maybe False isFirstUpper $ getLabelPrefix . citationId . head $ cits
    cref' | suppressAuthor = "\\labelcref"
          | cap = "\\Cref"
          | otherwise = "\\cref"

listLabels :: T.Text -> T.Text -> T.Text -> T.Text -> [Citation] -> T.Text
listLabels prefix p sep s =
  T.intercalate sep . map ((p <>) . (<> s) . mkLaTeXLabel' . (prefix<>) . getLabelWithoutPrefix . citationId)

getLabelWithoutPrefix :: T.Text -> T.Text
getLabelWithoutPrefix = T.drop 1 . T.dropWhile (/=':')

getLabelPrefix :: T.Text -> Maybe T.Text
getLabelPrefix lab
  | uncapitalizeFirst p `elem` prefixes = Just p
  | otherwise = Nothing
  where p = flip T.snoc ':' . T.takeWhile (/=':') $ lab

replaceRefsOther :: T.Text -> Options -> [Citation] -> WS [Inline]
replaceRefsOther prefix opts cits = toList . intercalate' (text ", ") . map fromList <$>
    mapM (replaceRefsOther' prefix opts) (groupBy citationGroupPred cits)

citationGroupPred :: Citation -> Citation -> Bool
citationGroupPred = (==) `on` liftM2 (,) citationPrefix citationMode

replaceRefsOther' :: T.Text -> Options -> [Citation] -> WS [Inline]
replaceRefsOther' prefix opts cits = do
  indices <- mapM (getRefIndex prefix opts) cits
  let
    cap = maybe False isFirstUpper $ getLabelPrefix . citationId . head $ cits
    writePrefix | all ((==SuppressAuthor) . citationMode) cits
                = id
                | all (null . citationPrefix) cits
                = cmap $ getRefPrefix opts prefix cap (length cits - 1)
                | otherwise
                = cmap $ toList . ((fromList (citationPrefix (head cits)) <> space) <>) . fromList
    cmap f [Link attr t w]
      | nameInLink opts = [Link attr (f t) w]
    cmap f x = f x
  return $ writePrefix (makeIndices opts indices)

data RefData = RefData { rdLabel :: T.Text
                       , rdIdx :: Maybe Index
                       , rdSubfig :: Maybe Index
                       , rdSuffix :: [Inline]
                       , rdTitle :: Maybe [Inline]
                       , rdPfx :: T.Text
                       } deriving (Eq)

instance Ord RefData where
  (<=) = (<=) `on` rdIdx

getRefIndex :: T.Text -> Options -> Citation -> WS RefData
getRefIndex prefix _opts Citation{citationId=cid,citationSuffix=suf}
  = do
    ref <- M.lookup lab <$> use prop
    let sub = refSubfigure <$> ref
        idx = refIndex <$> ref
        tit = refTitle <$> ref
    return RefData
      { rdLabel = lab
      , rdIdx = idx
      , rdSubfig = join sub
      , rdSuffix = suf
      , rdTitle = tit
      , rdPfx = prefix
      }
  where
  prop = lookupUnsafe prefix accMap
  lab = prefix <> getLabelWithoutPrefix cid

data RefItem = RefRange RefData RefData | RefSingle RefData

makeIndices :: Options -> [RefData] -> [Inline]
makeIndices o s = format $ concatMap f $ HT.groupBy g $ sort $ nub s
  where
  g :: RefData -> RefData -> Bool
  g a b = all (null . rdSuffix) [a, b] && (
            all (isNothing . rdSubfig) [a, b] &&
            Just True == (liftM2 follows `on` rdIdx) b a ||
            rdIdx a == rdIdx b &&
            Just True == (liftM2 follows `on` rdSubfig) b a
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
  show' RefData{rdLabel=l, rdIdx=Just i, rdSubfig = sub, rdSuffix = suf, rdTitle=tit, rdPfx=pfx}
    | linkReferences o = link ('#' `T.cons` l) "" (fromList txt)
    | otherwise = fromList txt
    where
      txt
        | Just sub' <- sub
        = let vars = M.fromDistinctAscList
                      [ ("i", chapPrefix (chapDelim o) i)
                      , ("s", chapPrefix (chapDelim o) sub')
                      , ("suf", suf)
                      , ("t", fromMaybe mempty tit)
                      ]
          in applyTemplate' vars $ subfigureRefIndexTemplate o
        | otherwise
        = let vars = M.fromDistinctAscList
                      [ ("i", chapPrefix (chapDelim o) i)
                      , ("suf", suf)
                      , ("t", fromMaybe mempty tit)
                      ]
          in applyTemplate' vars $ refIndexTemplate o (T.dropEnd 1 pfx)
  show' RefData{rdLabel=l, rdIdx=Nothing, rdSuffix = suf} =
    trace (T.unpack $ "Undefined cross-reference: " <> l)
          (strong (text $ "¿" <> l <> "?") <> fromList suf)
