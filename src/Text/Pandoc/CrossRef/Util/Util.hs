module Text.Pandoc.CrossRef.Util.Util where

import Text.Pandoc.Definition
import Data.Char (toUpper, toLower, isUpper)
import Data.List (intercalate)

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

chapPrefix :: [Inline] -> [Int] -> Int -> [Inline]
chapPrefix delim chap index = intercalate delim (map (return . Str . show) (chap++[index]))
