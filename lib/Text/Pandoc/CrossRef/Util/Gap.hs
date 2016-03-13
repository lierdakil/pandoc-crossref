module Text.Pandoc.CrossRef.Util.Gap where

import qualified Text.Pandoc as P

readMarkdown :: P.ReaderOptions -> String -> P.Pandoc
readMarkdown = (either (error . show) id .) . P.readMarkdown
