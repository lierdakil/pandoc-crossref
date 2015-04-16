module Util.Util where

import Text.Pandoc.Definition

isFormat :: String -> Format -> Bool
isFormat fmt (Format f) = takeWhile (`notElem` "+-") f == fmt
