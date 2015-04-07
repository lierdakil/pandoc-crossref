module Util.Util where

import Text.Pandoc.Definition
import qualified Data.Map as M
import Control.Monad (mplus)
import Util.Default.Default
import Util.Default.Types (DefaultSettings(..))

lookupDefault :: String -> Meta -> DefaultSettings -> Maybe MetaValue
lookupDefault name meta defMap =
  lookupMeta name meta `mplus` (MetaString `fmap` M.lookup name defMap) `mplus` getDefaultMeta name

isFormat :: String -> Format -> Bool
isFormat fmt (Format f) = takeWhile (`notElem` "+-") f == fmt
