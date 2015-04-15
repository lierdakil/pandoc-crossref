module Util.Util where

import Text.Pandoc.Definition
import Control.Monad (mplus)
import Util.Default.Default
import Data.Monoid

lookupDefault :: String -> Meta -> Meta -> Maybe MetaValue
lookupDefault name meta defMap =
  lookupMeta name meta `mplus` lookupMeta name (defMap `mappend` defaultMeta)

isFormat :: String -> Format -> Bool
isFormat fmt (Format f) = takeWhile (`notElem` "+-") f == fmt
