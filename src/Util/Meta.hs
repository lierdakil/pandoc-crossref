module Util.Meta where

import Text.Pandoc (readMarkdown,def)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Definition
import Data.Maybe (fromMaybe)
import Util.Util (lookupDefault)

import Util.Default.Types (DefaultSettings)

getMetaBool :: String -> Meta -> DefaultSettings -> Bool
getMetaBool name meta defaults = fromMaybe False $ lookupDefault name meta defaults >>= toBool

getMetaInlines :: String -> Meta -> DefaultSettings -> [Inline]
getMetaInlines name meta defaults = fromMaybe [] $ lookupDefault name meta defaults >>= toInlines

getMetaBlock :: String -> Meta -> DefaultSettings -> [Block]
getMetaBlock name meta defaults = fromMaybe [] $ lookupDefault name meta defaults >>= toBlocks

getMetaString :: String -> Meta -> DefaultSettings -> String
getMetaString name meta defaults = fromMaybe [] $ lookupDefault name meta defaults >>= toString

toInlines :: MetaValue -> Maybe [Inline]
toInlines (MetaString s) = return $ getInlines $ readMarkdown def s
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
toBlocks (MetaString s) = return $ getBlocks $ readMarkdown def s
  where getBlocks (Pandoc _ bs) = bs
toBlocks _ = Nothing

toString :: MetaValue -> Maybe String
toString (MetaString s) = Just s
toString (MetaBlocks b) = Just $ stringify b
toString (MetaInlines i) = Just $ stringify i
toString _ = Nothing
