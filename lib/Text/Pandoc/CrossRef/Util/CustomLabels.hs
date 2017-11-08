module Text.Pandoc.CrossRef.Util.CustomLabels (customLabel) where

import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Meta
import Data.List
import Text.Numeral.Roman

customLabel :: Meta -> String -> Int -> Maybe String
customLabel meta ref i
  | refLabel <- takeWhile (/=':') ref
  , Just cl <- lookupMeta (refLabel++"Labels") meta
  = mkLabel i (refLabel++"Labels") cl
  | otherwise = Nothing

mkLabel :: Int -> String -> MetaValue -> Maybe String
mkLabel i n lt
  | toString n lt == "arabic"
  = Nothing
  | toString n lt == "roman"
  = Just $ toRoman i
  | Just (startWith:_) <- stripPrefix "alpha " $ toString n lt
  = Just [[startWith..] !! (i-1)]
  | Just val <- toString n <$> getList (i-1) lt
  = Just val
  | otherwise = error $ "Unknown numeration type: " ++ show lt
