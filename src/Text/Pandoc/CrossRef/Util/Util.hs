module Text.Pandoc.CrossRef.Util.Util where

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.Definition
import Data.Char (toUpper, toLower, isUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

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
