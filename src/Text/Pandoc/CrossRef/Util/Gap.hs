{-# LANGUAGE CPP #-}

module Text.Pandoc.CrossRef.Util.Gap where

import qualified Text.Pandoc as P

readMarkdown :: P.ReaderOptions -> String -> P.Pandoc
#if MIN_VERSION_pandoc(1,14,0)
readMarkdown = (either (error . show) id .) . P.readMarkdown
#else
readMarkdown = P.readMarkdown
#endif
