module Util.Util where

import Text.Pandoc.Definition

isFormat :: String -> Maybe Format -> Bool
isFormat fmt (Just (Format f)) = takeWhile (`notElem` "+-") f == fmt
isFormat _ Nothing = False
