{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

{-# LANGUAGE FlexibleContexts, Rank2Types, UndecidableInstances, FlexibleInstances #-}
module Text.Pandoc.CrossRef.Util.Meta (
    getMetaList
  , getMetaBool
  , getMetaInlines
  , getMetaBlock
  , getMetaString
  , getMetaStringMaybe
  , getList
  , toString
  , toInlines
  , tryCapitalizeM
  ) where

import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.Default
import Text.Pandoc.Walk
import Text.Pandoc.Shared hiding (capitalize, toString)
import Data.Maybe

getMetaList :: (Default a) => (MetaValue -> a) -> String -> Settings -> Int -> a
getMetaList f name (Settings meta) i = maybe def f $ lookupMeta name meta >>= getList i

getMetaBool :: String -> Settings -> Bool
getMetaBool = getScalar toBool

getMetaInlines :: String -> Settings -> Inlines
getMetaInlines = getScalar toInlines

getMetaBlock :: String -> Settings -> Blocks
getMetaBlock = getScalar toBlocks

getMetaString :: String -> Settings -> String
getMetaString = getScalar toString

getMetaStringMaybe :: String -> Settings -> Maybe String
getMetaStringMaybe = getScalar toMaybeString

getScalar :: Def b => (String -> MetaValue -> b) -> String -> Settings -> b
getScalar conv name (Settings meta) = maybe def' (conv name) $ lookupMeta name meta

class Def a where
  def' :: a

instance Def Bool where
  def' = False

instance Def String where
  def' = []

instance Def (Maybe a) where
  def' = Nothing

instance (Monoid (Many a)) => Def (Many a) where
  def' = mempty

unexpectedError :: forall a. String -> String -> MetaValue -> a
unexpectedError e n x = error $ "Expected " <> e <> " in metadata field " <> n <> " but got " <> g x
  where
    g (MetaBlocks _) = "blocks"
    g (MetaString _) = "string"
    g (MetaInlines _) = "inlines"
    g (MetaBool _) = "bool"
    g (MetaMap _) = "map"
    g (MetaList _) = "list"

toInlines :: String -> MetaValue -> Inlines
toInlines _ (MetaBlocks s) = fromList $ blocksToInlines s
toInlines _ (MetaInlines s) = fromList s
toInlines _ (MetaString s) = text s
toInlines n x = unexpectedError "inlines" n x

toBool :: String -> MetaValue -> Bool
toBool _ (MetaBool b) = b
toBool n x = unexpectedError "bool" n x

toBlocks :: String -> MetaValue -> Blocks
toBlocks _ (MetaBlocks bs) = fromList bs
toBlocks _ (MetaInlines ils) = fromList [Plain ils]
toBlocks _ (MetaString s) = plain $ text s
toBlocks n x = unexpectedError "blocks" n x

toString :: String -> MetaValue -> String
toString n x = fromMaybe (unexpectedError "string" n x) $ toMaybeString n x

toMaybeString :: String -> MetaValue -> Maybe String
toMaybeString _ (MetaString s) = Just s
toMaybeString _ (MetaBlocks b) = Just $ stringify b
toMaybeString _ (MetaInlines i) = Just $ stringify i
toMaybeString _ _ = Nothing

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
