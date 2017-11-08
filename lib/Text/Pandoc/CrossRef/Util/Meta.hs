{-# LANGUAGE FlexibleContexts #-}
module Text.Pandoc.CrossRef.Util.Meta where

import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.Maybe (fromMaybe)
import Data.Default
import Text.Pandoc.Walk
import Text.Pandoc.Shared hiding (capitalize)

getMetaList :: (Default a) => (MetaValue -> Maybe a) -> String -> Meta -> Int -> a
getMetaList f name meta i = fromMaybe def $ lookupMeta name meta >>= getList i >>= f

getMetaBool :: String -> Meta -> Bool
getMetaBool name meta = fromMaybe False $ lookupMeta name meta >>= toBool

getMetaInlines :: String -> Meta -> [Inline]
getMetaInlines name meta = fromMaybe [] $ lookupMeta name meta >>= toInlines name

getMetaBlock :: String -> Meta -> [Block]
getMetaBlock name meta = fromMaybe [] $ lookupMeta name meta >>= toBlocks name

getMetaString :: String -> Meta -> String
getMetaString name meta = fromMaybe [] $ lookupMeta name meta >>= toString

toInlines :: String -> MetaValue -> Maybe [Inline]
toInlines _ (MetaBlocks s) = Just $ blocksToInlines s
toInlines _ (MetaInlines s) = return s
toInlines _ (MetaString s) = Just $ toList $ text s
toInlines _ _ = Nothing

toBool :: MetaValue -> Maybe Bool
toBool (MetaBool b) = return b
toBool _ = Nothing

toBlocks :: String -> MetaValue -> Maybe [Block]
toBlocks _ (MetaBlocks bs) = return bs
toBlocks _ (MetaInlines ils) = return [Plain ils]
toBlocks _ (MetaString s) = Just $ toList $ plain $ text s
toBlocks _ _ = Nothing

toString :: MetaValue -> Maybe String
toString (MetaString s) = Just s
toString (MetaBlocks b) = Just $ stringify b
toString (MetaInlines i) = Just $ stringify i
toString _ = Nothing

getList :: Int -> MetaValue -> Maybe MetaValue
getList i (MetaList l) = l !!? i
  where
    list !!? index | index >= 0 && index < length list = Just $ list !! index
                   | not $ null list = Just $ last list
                   | otherwise = Nothing
getList _ x = Just x

tryCapitalizeM :: (Functor m, Monad m, Walkable Inline a, Default a, Eq a) =>
        (String -> m a) -> String -> Bool -> m a
tryCapitalizeM f varname capitalize
  | capitalize = do
    res <- f (capitalizeFirst varname)
    case res of
      xs | xs == def -> f varname >>= walkM capStrFst
         | otherwise -> return xs
  | otherwise  = f varname
  where
    capStrFst (Str s) = return $ Str $ capitalizeFirst s
    capStrFst x = return x
