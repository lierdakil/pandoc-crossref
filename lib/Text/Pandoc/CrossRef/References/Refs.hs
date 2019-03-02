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

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

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
import Text.Pandoc.CrossRef.Util.Options hiding (getRefPrefix)
import Text.Pandoc.CrossRef.Util.Prefixes
import Text.Pandoc.CrossRef.Util.VarFunction
import Control.Applicative
import Debug.Trace
import Prelude

replaceRefs :: Options -> [Inline] -> WS [Inline]
replaceRefs opts ils
  | Cite cits _:xs <- ils
  = do
    refData <- mapM getRefData cits
    toList . (<> fromList xs) . intercalate' (text ", ") <$> mapM replaceRefs' (groupBy eqPred refData)
  where
    eqPred :: RefData -> RefData -> Bool
    eqPred = (==) `on` liftM2 (,) rdScope rdPrefix
    replaceRefs' refs
      | ref@RefDataComplete{} : _ <- refs
      = replaceRefs'' (fromJust $ rdPrefix ref) opts refs
      | otherwise = return $ cite cits' il'
        where
          cits' = mapMaybe getCit refs
          getCit RefDataIncomplete{rdCitation, rdiLabel}
            | takeWhile (/=':') rdiLabel `elem` M.keys (prefixes opts)
            = trace ("Undefined cross-reference: " ++ rdiLabel) $ Just rdCitation
            | otherwise = Just rdCitation
          getCit RefDataComplete{} = error "Converting RefDataComplete back to Citation. This should not happen, please report a bug"
          il' = str "["
             <> intercalate' (text "; ") (map citationToInlines cits')
             <> str "]"
          citationToInlines c =
            fromList (citationPrefix c) <> text ("@" ++ citationId c)
              <> fromList (citationSuffix c)
    replaceRefs''
      | isLatexFormat (outFormat opts) = replaceRefsLatex
      | otherwise = replaceRefsOther
replaceRefs _ x = return x

getRefPrefix :: Options -> Bool -> Int -> RefRec -> Inlines -> Inlines
getRefPrefix opts capitalize num rr@RefRec{..} cit =
  applyRefTemplate reftempl vf capitalize
  where Prefix{prefixReferenceTemplate=reftempl} = fromMaybe undefined $ M.lookup refPfx $ prefixes opts
        vf "rs" = Just cit
        vf "n" = Just $ str $ show num
        vf x = fix defaultVarFunc rr x

replaceRefsLatex :: String -> Options -> [RefData] -> WS Inlines
replaceRefsLatex prefix opts cits
  | cref opts
  = replaceRefsLatex' prefix opts cits
  | otherwise
  = intercalate' (text ", ") <$>
      mapM (replaceRefsLatex' prefix opts) (groupBy citationGroupPred cits)

replaceRefsLatex' :: String -> Options -> [RefData] -> WS Inlines
replaceRefsLatex' prefix opts cits =
  return $ p texcit
  where
    texcit = rawInline "tex" $
      if cref opts then
        cref'++"{"++listLabels prefix "" "," "" cits++"}"
        else
          listLabels prefix "\\ref{" ", " "}" cits
    RefDataComplete{..} = head cits
    p | cref opts = id
      | rdSuppressPrefix
      = id
      | isNothing rdCitPrefix
      = getRefPrefix opts cap (length cits - 1) rdRec
      | otherwise = ((fromJust rdCitPrefix <> space) <>)
    cap = maybe False isFirstUpper $ getLabelPrefix opts . rdLabel . head $ cits
    cref' | rdSuppressPrefix = "\\labelcref"
          | cap = "\\Cref"
          | otherwise = "\\cref"

listLabels :: String -> String -> String -> String -> [RefData] -> String
listLabels _prefix p sep s =
  intercalate sep . map ((p ++) . (++ s) . mkLaTeXLabel' . rdLabel)

getLabelPrefix :: Options -> String -> Maybe String
getLabelPrefix opts lab
  | uncapitalizeFirst p `elem` prefixList opts = Just p
  | otherwise = Nothing
  where p = takeWhile (/=':') lab

replaceRefsOther :: String -> Options -> [RefData] -> WS Inlines
replaceRefsOther prefix opts cits = intercalate' (text ", ") <$>
    mapM (replaceRefsOther' prefix opts) (groupBy citationGroupPred cits)

citationGroupPred :: RefData -> RefData -> Bool
citationGroupPred = (==) `on` liftM2 (,) rdCitPrefix rdSuppressPrefix

replaceRefsOther' :: String -> Options -> [RefData] -> WS Inlines
replaceRefsOther' _prefix opts indices = do
  let
    RefDataComplete{..} = head indices
    cap = rdUpperCase
    writePrefix | rdSuppressPrefix
                = id
                | isNothing rdCitPrefix
                = cmap $ getRefPrefix opts cap (length indices - 1) rdRec
                | otherwise
                = cmap ((fromJust rdCitPrefix <> space) <>)
    cmap f x
      | nameInLink opts
      , [Link attr t (y, z)] <- toList x = linkWith attr y z (f $ fromList t)
    cmap f x = f x
  return $ writePrefix (makeIndices opts indices)

data RefData = RefDataIncomplete
             { rdiLabel :: String
             , rdSuffix :: Inlines
             , rdCitation :: Citation
             }
             | RefDataComplete
             { rdRec :: RefRec
             , rdSuffix :: Inlines
             , rdCitPrefix :: Maybe Inlines
             , rdUpperCase :: Bool
             , rdSuppressPrefix :: Bool
             } deriving (Eq, Show)

rdIdx :: RefData -> Maybe Int
rdIdx RefDataIncomplete{} = Nothing
rdIdx RefDataComplete{rdRec} = Just $ refIndex rdRec

rdScope :: RefData -> Maybe RefRec
rdScope RefDataIncomplete{} = Nothing
rdScope RefDataComplete{rdRec} = refScope rdRec

rdPrefix :: RefData -> Maybe String
rdPrefix RefDataIncomplete{} = Nothing
rdPrefix RefDataComplete{rdRec} = Just $ refPfx rdRec

rdLabel :: RefData -> String
rdLabel RefDataIncomplete{rdiLabel} = rdiLabel
rdLabel RefDataComplete{rdRec} = refLabel rdRec

instance Ord RefData where
  (<=) = (<=) `on` rdIdx

getRefData :: Citation -> WS RefData
getRefData c@Citation{..}
  = do
    ref <- M.lookup llab <$> get referenceData
    return $ case ref of
      Nothing -> RefDataIncomplete
        { rdiLabel = llab
        , rdSuffix = suf'
        , rdCitation = c
        }
      Just x -> RefDataComplete
        { rdRec = x
        , rdSuffix = suf'
        , rdCitPrefix = if null citationPrefix
                        then Nothing
                        else Just $ fromList citationPrefix
        , rdUpperCase = isFirstUpper citationId
        , rdSuppressPrefix = SuppressAuthor == citationMode
        }
    where llab = uncapitalizeFirst citationId
          suf' = fromList citationSuffix

data RefItem = RefRange RefData RefData | RefSingle RefData

makeIndices :: Options -> [RefData] -> Inlines
makeIndices o s = format $ concatMap f $ HT.groupBy g $ sort $ nub s
  where
  g :: RefData -> RefData -> Bool
  g a b = all (null . rdSuffix) [a, b] && (
            fromMaybe False ((liftM2 follows `on` rdIdx) b a) &&
            ((==) `on` rdScope) a b
          )
  follows :: Int -> Int -> Bool
  follows a b = b + 1 == a
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
  show' RefDataComplete{..}
    | linkReferences o = link ('#':refLabel rdRec) "" txt
    | otherwise = txt
    where txt = applyIndexTemplate o rdSuffix rdRec
  show' RefDataIncomplete{..} =
    error ("Undefined cross-reference: " <> rdiLabel
        <> ". This should not be possible, please report a bug")

applyIndexTemplate :: Options -> Many Inline -> RefRec -> Inlines
applyIndexTemplate opts suf rr =
  let varsSc rr' "ref" = Just $ inlines False rr'
      varsSc rr' "Ref" = Just $ inlines True rr'
      varsSc rr' x = defaultVarFunc varsSc rr' x
      vars _ "suf" = Just suf
      vars rr' x = defaultVarFunc varsSc rr' x
      template = prefixReferenceIndexTemplate pfxRec
      pfxRec = getPfx opts (refPfx rr)
      inlines cap ref =
        getRefPrefix opts cap 0 ref $ applyIndexTemplate opts mempty ref
  in applyTemplate (vars rr) template
