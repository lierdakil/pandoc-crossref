module Util.Meta where

import Text.Pandoc (readMarkdown,def)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Definition
import Data.Maybe (fromMaybe)

getMetaBool :: String -> Meta -> Bool
getMetaBool name meta = fromMaybe False $ lookupMeta name meta >>= toBool

getMetaInlines :: String -> Meta -> [Inline]
getMetaInlines name meta = fromMaybe [] $ lookupMeta name meta >>= toInlines

getMetaBlock :: String -> Meta -> [Block]
getMetaBlock name meta = fromMaybe [] $ lookupMeta name meta >>= toBlocks

getMetaString :: String -> Meta -> String
getMetaString name meta = fromMaybe [] $ lookupMeta name meta >>= toString

toInlines :: MetaValue -> Maybe [Inline]
toInlines (MetaString s) = return $ getInlines $
  either (error . show) id $ readMarkdown def s
  where getInlines (Pandoc _ bs) = concatMap getInline bs
        getInline (Plain ils) = ils
        getInline (Para ils) = ils
        getInline _ = []
toInlines (MetaInlines s) = return s
toInlines _ = Nothing

toBool :: MetaValue -> Maybe Bool
toBool (MetaBool b) = return b
toBool _ = Nothing

toBlocks :: MetaValue -> Maybe [Block]
toBlocks (MetaBlocks bs) = return bs
toBlocks (MetaInlines ils) = return [Plain ils]
toBlocks (MetaString s) = return $ getBlocks $
  either (error . show) id $ readMarkdown def s
  where getBlocks (Pandoc _ bs) = bs
toBlocks _ = Nothing

toString :: MetaValue -> Maybe String
toString (MetaString s) = Just s
toString (MetaBlocks b) = Just $ stringify b
toString (MetaInlines i) = Just $ stringify i
toString _ = Nothing
