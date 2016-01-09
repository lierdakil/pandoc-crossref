module Text.Pandoc.CrossRef.Util.Settings.Template where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import qualified Data.Map as M

template :: ToMetaValue a => String -> a -> Meta
template name = Meta . M.singleton name . toMetaValue
