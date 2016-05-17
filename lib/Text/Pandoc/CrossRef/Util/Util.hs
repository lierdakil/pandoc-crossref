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

-- | Monadic variation on everywhere'
everywhereMBut' :: Monad m => GenericQ Bool -> GenericM m -> GenericM m

-- Top-down order is also reflected in order of do-actions
everywhereMBut' q f x
  | q x = f x
  | otherwise = do
    x' <- f x
    if q x'
    then return x'
    else gmapM (everywhereMBut' q f) x'

mkLaTeXLabel :: String -> String
mkLaTeXLabel l = "\\label{" ++ mkLaTeXLabel' l ++ "}"

mkLaTeXLabel' :: String -> String
mkLaTeXLabel' l =
  let ll = writeLaTeX def $ Pandoc nullMeta [Div (l, [], []) []]
  in takeWhile (/='}') . drop 1 . dropWhile (/='{') $ ll
