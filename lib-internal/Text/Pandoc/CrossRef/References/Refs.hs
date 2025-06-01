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

import Control.Arrow as A
import Data.Function
import Numeric.Natural
import Data.List
import qualified Data.List.HT as HT
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Text.Pandoc.Builder
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..))
import Control.Monad (liftM2, join)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Debug.Trace
import Lens.Micro
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util

replaceRefs :: Inline -> Options -> References -> Maybe Inlines
replaceRefs (Cite cits _) opts = Just .
  intercalate' (text ", ") . map fromList <$> mapM replaceRefs' (NE.groupBy eqPrefix cits)
  where
    eqPrefix = (==) `on` getLabelPrefix . citationId
    replaceRefs' :: NonEmpty Citation -> References -> [Inline]
    replaceRefs' cits'
      | Just prefix <- getLabelPrefix . citationId $ NE.head cits'
      = replaceRefs'' prefix cits'
      | otherwise = return [Cite (NE.toList cits') il']
        where
          il' = toList $
              str "["
            <> intercalate' (text "; ") (map citationToInlines $ NE.toList cits')
            <> str "]"
          citationToInlines c =
            fromList (citationPrefix c) <> text ("@" <> citationId c)
              <> fromList (citationSuffix c)
    replaceRefs'' :: Prefix -> NonEmpty Citation -> References -> [Inline]
    replaceRefs'' = ($ opts) . flip $
      if isLatexFormat opts
      then replaceRefsLatex
      else replaceRefsOther
replaceRefs _ _ = pure Nothing

-- accessors to options
prefMap :: Prefix -> (Options -> Bool -> Int -> [Inline], Options -> Template)
prefMap = \case
  PfxImg -> (figPrefix, figPrefixTemplate)
  PfxEqn -> (eqnPrefix, eqnPrefixTemplate)
  PfxTbl -> (tblPrefix, tblPrefixTemplate)
  PfxLst -> (lstPrefix, lstPrefixTemplate)
  PfxSec -> (secPrefix, secPrefixTemplate)

prefixes :: [Prefix]
prefixes = [minBound..]

getRefPrefix :: Options -> Prefix -> Bool -> Int -> [Inline] -> [Inline]
getRefPrefix opts prefix capitalize num cit =
  applyTemplate' (M.fromDistinctAscList [("i", cit), ("p", refprefix)])
        $ reftempl opts
  where (refprefixf, reftempl) = prefMap prefix
        refprefix = refprefixf opts capitalize num

replaceRefsLatex :: Prefix -> Options -> NonEmpty Citation -> References -> [Inline]
replaceRefsLatex prefix opts cits
  | cref opts
  = replaceRefsLatex' prefix opts cits
  | otherwise
  = toList . intercalate' (text ", ") . map fromList <$>
      mapM (replaceRefsLatex' prefix opts) (NE.groupBy citationGroupPred cits)

replaceRefsLatex' :: Prefix -> Options -> NonEmpty Citation -> References -> [Inline]
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
      | otherwise = ((citationPrefix (NE.head cits) <> [Space]) <>)
    cap = isFirstUpper . citationId . NE.head $ cits
    cref' | suppressAuthor = "\\labelcref"
          | cap = "\\Cref"
          | otherwise = "\\cref"

listLabels :: Prefix -> T.Text -> T.Text -> T.Text -> NonEmpty Citation -> T.Text
listLabels prefix p sep s
  = T.intercalate sep
  . map ((p <>) . (<> s) . mkLaTeXLabel' . (pfxTextCol prefix <>) . getLabelWithoutPrefix . citationId)
  . NE.toList


getLabelWithoutPrefix :: T.Text -> T.Text
getLabelWithoutPrefix = T.drop 1 . T.dropWhile (/=':')

getLabelPrefix :: T.Text -> Maybe Prefix
getLabelPrefix lab = find (p `hasPfx`) prefixes
  where
    p = uncapitalizeFirst $ flip T.snoc ':' . T.takeWhile (/=':') $ lab

replaceRefsOther :: Prefix -> Options -> NonEmpty Citation -> References -> [Inline]
replaceRefsOther prefix opts cits = toList . intercalate' (text ", ") . map fromList <$>
    traverse (replaceRefsOther' prefix opts) (NE.groupBy citationGroupPred cits)

citationGroupPred :: Citation -> Citation -> Bool
citationGroupPred = (==) `on` liftM2 (,) citationPrefix citationMode

replaceRefsOther' :: Prefix -> Options -> NonEmpty Citation -> References -> [Inline]
replaceRefsOther' prefix opts cits refs =
  let
    indices = getRefIndex prefix refs <$> NE.toList cits
    cap = isFirstUpper . citationId . NE.head $ cits
    writePrefix | all ((==SuppressAuthor) . citationMode) cits
                = id
                | all (null . citationPrefix) cits
                = cmap $ getRefPrefix opts prefix cap (length cits - 1)
                | otherwise
                = cmap $ toList . ((fromList (citationPrefix (NE.head cits)) <> space) <>) . fromList
    cmap f [Link attr t w]
      | nameInLink opts = [Link attr (f t) w]
    cmap f x = f x
  in writePrefix (makeIndices opts indices)

data RefData = RefData { rdGlob :: Maybe Natural
                       , rdLabel :: T.Text
                       , rdIdx :: Maybe Index
                       , rdSubfig :: Maybe Index
                       , rdSuffix :: [Inline]
                       , rdTitle :: Maybe [Inline]
                       , rdPfx :: Prefix
                       } deriving (Eq)

getRefIndex :: Prefix -> References -> Citation -> RefData
getRefIndex prefix refs Citation{citationId=cid,citationSuffix=suf}
  = let ref = M.lookup lab $ refs ^. prop
        sub = refSubfigure <$> ref
        idx = refIndex <$> ref
        tit = refTitle <$> ref
        glob = refGlobal <$> ref
    in RefData
      { rdGlob = glob
      , rdLabel = lab
      , rdIdx = idx
      , rdSubfig = join sub
      , rdSuffix = suf
      , rdTitle = tit
      , rdPfx = prefix
      }
  where
  prop = refsAt prefix
  lab = pfxTextCol prefix <> getLabelWithoutPrefix cid

data RefItem = RefRange RefData RefData | RefSingle RefData

makeIndices :: Options -> [RefData] -> [Inline]
makeIndices o = format . concatMap f . HT.groupBy g . sortOn rdGlob . nub
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
    | ai :> al <- S.viewr a
    , bi :> bl <- S.viewr b
    = ai == bi && A.first succ bl == al
    | otherwise = False
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
          in applyTemplate' vars $ refIndexTemplate o pfx
  show' RefData{rdLabel=l, rdIdx=Nothing, rdSuffix = suf} =
    trace (T.unpack $ "Undefined cross-reference: " <> l)
          (strong (text $ "¿" <> l <> "?") <> fromList suf)
