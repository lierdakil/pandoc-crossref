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

import Data.Accessor.Monad.Trans.State
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Prefixes
import Control.Applicative
import Data.Char (toLower)
import Debug.Trace
import Prelude

replaceRefs :: Options -> [Inline] -> WS [Inline]
replaceRefs opts ils
  | Cite cits _:xs <- ils
  = toList . (<> fromList xs) . intercalate' (text ", ") <$> mapM replaceRefs' (groupBy eqPrefix cits)
  where
    eqPrefix a b = uncurry (==) $
      (fmap uncapitalizeFirst . getLabelPrefix opts . citationId) <***> (a,b)
    (<***>) = join (***)
    replaceRefs' cits'
      | Just prefix <- allCitsPrefix opts cits'
      = replaceRefs'' prefix opts cits'
      | otherwise = return $ cite cits' il'
        where
          il' = str "["
             <> intercalate' (text "; ") (map citationToInlines cits')
             <> str "]"
          citationToInlines c =
            fromList (citationPrefix c) <> text ("@" ++ citationId c)
              <> fromList (citationSuffix c)
    replaceRefs'' = case outFormat opts of
                    f | isLatexFormat f -> replaceRefsLatex
                    _                      -> replaceRefsOther
replaceRefs _ x = return x

getRefPrefix :: Options -> String -> Bool -> Int -> Inlines -> Inlines
getRefPrefix opts prefix capitalize num cit =
  applyTemplate' (M.fromDistinctAscList [("i", cit), ("p", refprefix)]) reftempl
  where Prefix{prefixRef=refprefixf, prefixReferenceTemplate=reftempl} = fromMaybe undefined $ M.lookup prefix $ prefixes opts
        refprefix = refprefixf capitalize num

allCitsPrefix :: Options -> [Citation] -> Maybe String
allCitsPrefix opts cits = find isCitationPrefix $ prefixList opts
  where
  isCitationPrefix p =
    all (p `isPrefixOf`) $ map (uncapitalizeFirst . citationId) cits

replaceRefsLatex :: String -> Options -> [Citation] -> WS Inlines
replaceRefsLatex prefix opts cits
  | cref opts
  = replaceRefsLatex' prefix opts cits
  | otherwise
  = intercalate' (text ", ") <$>
      mapM (replaceRefsLatex' prefix opts) (groupBy citationGroupPred cits)

replaceRefsLatex' :: String -> Options -> [Citation] -> WS Inlines
replaceRefsLatex' prefix opts cits =
  return $ p texcit
  where
    texcit = rawInline "tex" $
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
      | otherwise = ((fromList (citationPrefix (head cits)) <> space) <>)
    cap = maybe False isFirstUpper $ getLabelPrefix opts . citationId . head $ cits
    cref' | suppressAuthor = "\\labelcref"
          | cap = "\\Cref"
          | otherwise = "\\cref"

listLabels :: String -> String -> String -> String -> [Citation] -> String
listLabels _prefix p sep s =
  intercalate sep . map ((p ++) . (++ s) . mkLaTeXLabel' . citationId)

getLabelPrefix :: Options -> String -> Maybe String
getLabelPrefix opts lab
  | uncapitalizeFirst p `elem` prefixList opts = Just p
  | otherwise = Nothing
  where p = takeWhile (/=':') lab

replaceRefsOther :: String -> Options -> [Citation] -> WS Inlines
replaceRefsOther prefix opts cits = intercalate' (text ", ") <$>
    mapM (replaceRefsOther' prefix opts) (groupBy citationGroupPred cits)

citationGroupPred :: Citation -> Citation -> Bool
citationGroupPred = (==) `on` liftM2 (,) citationPrefix citationMode

replaceRefsOther' :: String -> Options -> [Citation] -> WS Inlines
replaceRefsOther' prefix opts cits = do
  indices <- mapM getRefIndex cits
  let
    cap = maybe False isFirstUpper $ getLabelPrefix opts . citationId . head $ cits
    writePrefix | all (==SuppressAuthor) $ map citationMode cits
                = id
                | all null $ map citationPrefix cits
                = cmap $ getRefPrefix opts prefix cap (length cits - 1)
                | otherwise
                = cmap ((fromList (citationPrefix (head cits)) <> space) <>)
    cmap f x
      | nameInLink opts
      , [Link attr t (y, z)] <- toList x = linkWith attr y z (f $ fromList t)
    cmap f x = f x
  return $ writePrefix (makeIndices opts indices)

data RefData = RefData { rdLabel :: String
                       , rdIdx :: Maybe Index
                       , rdSubfig :: Maybe Index
                       , rdSuffix :: Inlines
                       } deriving (Eq, Show)

instance Ord RefData where
  (<=) = (<=) `on` rdIdx

getRefIndex :: Citation -> WS RefData
getRefIndex Citation{citationId=cid,citationSuffix=suf}
  = do
    ref <- M.lookup llab <$> get referenceData
    let sub = refSubfigure <$> ref
        idx = refIndex <$> ref
    return RefData
      { rdLabel = llab
      , rdIdx = idx
      , rdSubfig = join sub
      , rdSuffix = fromList suf
      }
  where
  (pfx, lab) = span (/=':') cid
  lpfx = map toLower pfx
  llab = lpfx <> lab

data RefItem = RefRange RefData RefData | RefSingle RefData

makeIndices :: Options -> [RefData] -> Inlines
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
    = ai == bi && fst bl + 1 == fst al
  follows _ _ = False
  f :: [RefData] -> [RefItem]
  f []  = []                          -- drop empty lists
  f [w] = [RefSingle w]                   -- single value
  f [w1,w2] = [RefSingle w1, RefSingle w2] -- two values
  f (x:xs) = [RefRange x (last xs)] -- shorten more than two values
  format :: [RefItem] -> Inlines
  format [] = mempty
  format [x] = show'' x
  format [x, y] = show'' x <> pairDelim o <> show'' y
  format xs = intercalate' (refDelim o) init' <> lastDelim o <> last'
    where initlast []     = error "emtpy list in initlast"
          initlast [y]    = ([], y)
          initlast (y:ys) = first (y:) $ initlast ys
          (init', last') = initlast $ map show'' xs
  show'' :: RefItem -> Inlines
  show'' (RefSingle x) = show' x
  show'' (RefRange x y) = show' x <> rangeDelim o <> show' y
  show' :: RefData -> Inlines
  show' RefData{rdLabel=l, rdIdx=Just i, rdSubfig = sub, rdSuffix = suf}
    | linkReferences o = link ('#':l) "" txt
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
          (strong (text $ "¿" ++ l ++ "?") <> suf)
