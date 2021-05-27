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

{-# LANGUAGE RecordWildCards, NamedFieldPuns, FlexibleInstances,
  FlexibleContexts, OverloadedStrings #-}

module Text.Pandoc.CrossRef.References.Refs (replaceRefs) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Control.Monad.State hiding (get, modify)
import Data.List
import qualified Data.List.HT as HT
import qualified Data.Text as T
import Data.Maybe
import Data.Function
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow as A

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Template
import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Options hiding (getRefPrefix)
import Text.Pandoc.CrossRef.Util.Prefixes
import Text.Pandoc.CrossRef.Util.VarFunction
import Control.Applicative
import Data.Either
import Prelude

groupEither :: [Either a b] -> [Either [a] [b]]
groupEither [] = []
groupEither (Left x:xs)
  = Left (x:lefts ys) : groupEither zs
  where (ys,zs) = span isLeft xs
groupEither (Right x:xs)
  = Right (x:rights ys) : groupEither zs
  where (ys,zs) = span isRight xs

replaceRefs :: [Inline] -> WS [Inline]
replaceRefs ils
  | Cite cits _:xs <- ils
  = do
    opts <- asks creOptions
    let
      eqPred :: RefDataComplete -> RefDataComplete -> Bool
      eqPred = (==) `on` liftM2 (,) rdLevel rdPrefix
      intrclt = intercalate' (text ", ")
      replaceRefs' (Left xs') = restoreCits' xs'
      replaceRefs' (Right xs') = intrclt <$> mapM (replaceRefsOther opts) (NE.groupBy eqPred xs')
      restoreCits' refs = liftM2 cite cits' il'
          where
            cits' = mapM getCit refs
            getCit RefDataIncomplete{rdCitation, rdiLabel}
              | T.takeWhile (/=':') rdiLabel `elem` M.keys (prefixes opts)
              = tell ["Undefined cross-reference: " <> rdiLabel] >> return rdCitation
              | otherwise = return rdCitation
            il' = do
              i <- map citationToInlines <$> cits'
              return $ str "["
               <> intercalate' (text "; ") i
               <> str "]"
            citationToInlines c =
              fromList (citationPrefix c) <> text ("@" <> citationId c)
                <> fromList (citationSuffix c)
    citRefData <- groupEither <$> mapM getRefData cits
    toList . (<> fromList xs) . intrclt <$> mapM replaceRefs' citRefData
replaceRefs x = return x

getRefPrefix :: Bool -> Int -> RefRec -> Inlines -> Inlines
getRefPrefix capitalize num rr@RefRec{..} cit =
  applyRefTemplate reftempl vf capitalize
  where Prefix{prefixReferenceTemplate=reftempl} = refPfxRec
        vf "rs" = Just $ MetaInlines $ toList cit
        vf "n" = Just $ MetaString $ T.pack $ show num
        vf x = fix defaultVarFunc rr x

replaceRefsOther :: Options -> NonEmpty RefDataComplete -> WS Inlines
replaceRefsOther opts cits = intercalate' (text ", ") <$>
    mapM (replaceRefsOther' opts) (NE.groupBy citationGroupPred cits)

citationGroupPred :: RefDataComplete -> RefDataComplete -> Bool
citationGroupPred = (==) `on` liftM2 (,) rdCitPrefix rdSuppressPrefix

replaceRefsOther' :: Options -> NonEmpty RefDataComplete -> WS Inlines
replaceRefsOther' opts indices = do
  let
    cmap f x
      | nameInLink opts
      , [Link attr t (y, z)] <- toList x = linkWith attr y z (f $ fromList t)
    cmap f x = f x
  return $ cmap (writePrefix indices) (makeIndices opts indices)

writePrefix :: NonEmpty RefDataComplete -> Inlines -> Inlines
writePrefix (RefDataComplete{..}:|rds)
  | rdSuppressPrefix = id
  | isNothing rdCitPrefix = getRefPrefix rdUpperCase (length rds) rdRec
  | otherwise = ((fromJust rdCitPrefix <> space) <>)

data RefDataIncomplete = RefDataIncomplete
                       { rdiLabel :: T.Text
                       , rdiSuffix :: Inlines
                       , rdCitation :: Citation
                       }
data RefDataComplete = RefDataComplete
                     { rdRec :: RefRec
                     , rdcSuffix :: Inlines
                     , rdCitPrefix :: Maybe Inlines
                     , rdUpperCase :: Bool
                     , rdSuppressPrefix :: Bool
                     }

type RefData = Either RefDataIncomplete RefDataComplete

rdIdx :: RefDataComplete -> Int
rdIdx RefDataComplete{rdRec} = refIndex rdRec

rdScope :: RefDataComplete -> Maybe RefRec
rdScope RefDataComplete{rdRec} = refScope rdRec

rdPrefix :: RefDataComplete -> T.Text
rdPrefix RefDataComplete{rdRec} = refPfx rdRec

rdLevel :: RefDataComplete -> Int
rdLevel RefDataComplete{rdRec} = refLevel rdRec

instance Eq RefDataComplete where
  (==) = (==) `on` rdRec

instance Ord RefDataComplete where
  (<=) = (<=) `on` rdRec

getRefData :: Citation -> WS RefData
getRefData c@Citation{..}
  = do
    ref <- M.lookup llab <$> get referenceData
    return $ case ref of
      Nothing -> Left $ RefDataIncomplete
        { rdiLabel = llab
        , rdiSuffix = suf'
        , rdCitation = c
        }
      Just x -> Right $ RefDataComplete
        { rdRec = x
        , rdcSuffix = suf'
        , rdCitPrefix = if null citationPrefix
                        then Nothing
                        else Just $ fromList citationPrefix
        , rdUpperCase = isFirstUpper citationId
        , rdSuppressPrefix = SuppressAuthor == citationMode
        }
    where llab = uncapitalizeFirst citationId
          suf' = fromList citationSuffix

data RefItem = RefRange RefDataComplete RefDataComplete | RefSingle RefDataComplete

makeIndices :: Options -> NonEmpty RefDataComplete -> Inlines
makeIndices o s = format $ concatMap f $ HT.groupBy g $ sort $ nub $ NE.toList s
  where
  g :: RefDataComplete -> RefDataComplete -> Bool
  g a b = all (null . rdcSuffix) [a, b]
          && (follows `on` rdIdx) b a
          && ((==) `on` rdScope) a b
  follows :: Int -> Int -> Bool
  follows a b = b + 1 == a
  f :: [RefDataComplete] -> [RefItem]
  f []  = []                          -- drop empty lists
  f [w] = [RefSingle w]                   -- single value
  f [w1,w2] = [RefSingle w1, RefSingle w2] -- two values
  f (x:xs) = [RefRange x (last xs)] -- shorten more than two values
  format :: [RefItem] -> Inlines
  format [] = mempty
  format [x] = show'' x
  format [x, y] = show'' x <> pairDelim o <> show'' y
  format (x:xs) = intercalate' (refDelim o) init' <> lastDelim o <> last'
    where initlast (y :| []) = ([], y)
          initlast (y :| (z:zs)) = first (y:) $ initlast (z:|zs)
          (init', last') = initlast $ NE.map show'' (x:|xs)
  show'' :: RefItem -> Inlines
  show'' (RefSingle x) = show' x
  show'' (RefRange x y) = show' x <> rangeDelim o <> show' y
  show' :: RefDataComplete -> Inlines
  show' RefDataComplete{..}
    | linkReferences o = link ('#' `T.cons` refLabel rdRec) "" txt
    | otherwise = txt
    where txt = applyIndexTemplate rdcSuffix rdRec

applyIndexTemplate :: Inlines -> RefRec -> Inlines
applyIndexTemplate suf rr =
  let varsSc rr' "ref" = Just $ inlines False rr'
      varsSc rr' "Ref" = Just $ inlines True rr'
      varsSc rr' "refi" = Just . MetaInlines . toList $ applyIndexTemplate mempty rr'
      varsSc rr' x = defaultVarFunc varsSc rr' x
      vars _ "suf" = Just $ MetaInlines $ toList suf
      vars rr' x = defaultVarFunc varsSc rr' x
      template = prefixReferenceIndexTemplate $ refPfxRec rr
      inlines cap ref = MetaInlines $ toList $
        getRefPrefix cap 0 ref $ applyIndexTemplate mempty ref
  in applyTemplate template (vars rr)
