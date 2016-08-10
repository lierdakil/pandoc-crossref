{-# LANGUAGE RankNTypes #-}
module Text.Pandoc.CrossRef.Util.Util
  ( module Text.Pandoc.CrossRef.Util.Util
  , module Data.Generics
  ) where

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.Definition
import Data.Char (toUpper, toLower, isUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Generics
import Text.Pandoc.Writers.LaTeX
import Data.Default

isFormat :: String -> Maybe Format -> Bool
isFormat fmt (Just (Format f)) = takeWhile (`notElem` "+-") f == fmt
isFormat _ Nothing = False

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs
capitalizeFirst [] = []

uncapitalizeFirst :: String -> String
uncapitalizeFirst (x:xs) = toLower x : xs
uncapitalizeFirst [] = []

isFirstUpper :: String -> Bool
isFirstUpper (x:_) = isUpper x
isFirstUpper [] = False

chapPrefix :: [Inline] -> Index -> [Inline]
chapPrefix delim index = intercalate delim (map (return . Str . uncurry (fromMaybe . show)) index)

runReplace :: (Monad m) => GenericM m -> GenericM m
runReplace f x = do
  x' <- f x
  if x' `geq` x
  then gmapM (runReplace f) x'
  else return x'

mkLaTeXLabel :: String -> String
mkLaTeXLabel l
 | null l = []
 | otherwise = "\\label{" ++ mkLaTeXLabel' l ++ "}"

mkLaTeXLabel' :: String -> String
mkLaTeXLabel' l =
  let ll = writeLaTeX def $ Pandoc nullMeta [Div (l, [], []) []]
  in takeWhile (/='}') . drop 1 . dropWhile (/='{') $ ll
