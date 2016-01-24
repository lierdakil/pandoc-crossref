module Text.Pandoc.CrossRef.Util.CustomLabels (customLabel) where

import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Meta
import Control.Monad
import Data.List
import Text.Numeral.Roman

customLabel :: Meta -> String -> Int -> Maybe String
customLabel meta ref i
  | refLabel <- takeWhile (/=':') ref
  , Just cl <- lookupMeta (refLabel++"Labels") meta
  = mkLabel i cl
  | otherwise = Nothing

mkLabel :: Int -> MetaValue -> Maybe String
mkLabel i lt
  | toString lt == Just "arabic"
  = Nothing
  | toString lt == Just "roman"
  = Just $ toRoman i
  | Just (startWith:_) <- join $ stripPrefix "alpha " `fmap` toString lt
  = Just [[startWith..] !! (i-1)]
  | Just val <- join $ toString `fmap` getList (i-1) lt
  = Just val
  | otherwise = error $ "Unknown numeration type: " ++ show lt
