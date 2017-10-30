module Text.Pandoc.CrossRef.Util.Gap where

import qualified Text.Pandoc as P
import Data.Text
import Text.Pandoc.Class

readMarkdown :: P.ReaderOptions -> Text -> P.Pandoc
readMarkdown o = either (error . show) id . runPure . P.readMarkdown o
