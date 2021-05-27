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

{-# LANGUAGE FlexibleContexts, Rank2Types, UndecidableInstances
           , FlexibleInstances, OverloadedStrings #-}
module Text.Pandoc.CrossRef.Util.Meta (
    getMetaList
  , getMetaBool
  , getMetaBoolDefault
  , getMetaInlines
  , getMetaBlock
  , getMetaString
  , getMetaStringMaybe
  , getMetaStringList
  , getList
  , getObj
  , toString
  , toInlines
  , capitalize
  ) where

import Text.Pandoc.CrossRef.Util.Util
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.Default
import Text.Pandoc.Walk
import Text.Pandoc.Shared hiding (capitalize)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

getMetaList :: (Default a) => (MetaValue -> a) -> T.Text -> Settings -> Int -> a
getMetaList f name (Settings meta) i = maybe def f $ lookupMeta name meta >>= getList i

getMetaStringList :: T.Text -> Settings -> [T.Text]
getMetaStringList name (Settings meta) = maybe [] (getList' name) $ lookupMeta name meta
  where
    getList' n (MetaList l) = map (toString n) l
    getList' n x = [toString n x]

getMetaBool :: T.Text -> Settings -> Bool
getMetaBool = getScalar toBool

getMetaBoolDefault :: T.Text -> Settings -> Bool -> Bool
getMetaBoolDefault = getScalarDefault toBool

getMetaInlines :: T.Text -> Settings -> Inlines
getMetaInlines = getScalar toInlines

getMetaBlock :: T.Text -> Settings -> Blocks
getMetaBlock = getScalar toBlocks

getMetaString :: T.Text -> Settings -> T.Text
getMetaString = getScalar toString

getMetaStringMaybe :: T.Text -> Settings -> Maybe T.Text
getMetaStringMaybe = getScalar (const toMaybeString)

getScalar :: Def b => (T.Text -> MetaValue -> b) -> T.Text -> Settings -> b
getScalar conv name (Settings meta) = maybe def' (conv name) $ lookupMeta name meta

getScalarDefault :: (T.Text -> MetaValue -> b) -> T.Text -> Settings -> b -> b
getScalarDefault conv name (Settings meta) dv = maybe dv (conv name) $ lookupMeta name meta

class Def a where
  def' :: a

instance Def Bool where
  def' = False

instance Def T.Text where
  def' = ""

instance Def (Maybe a) where
  def' = Nothing

instance (Monoid (Many a)) => Def (Many a) where
  def' = mempty

unexpectedError :: forall a. T.Text -> T.Text -> MetaValue -> a
unexpectedError e n x = error . T.unpack $ "Expected " <> e <> " in metadata field " <> n <> " but got " <> g x
  where
    g (MetaBlocks _) = "blocks"
    g (MetaString _) = "string"
    g (MetaInlines _) = "inlines"
    g (MetaBool _) = "bool"
    g (MetaMap _) = "map"
    g (MetaList _) = "list"

toInlines :: T.Text -> MetaValue -> Inlines
toInlines _ (MetaBlocks s) = fromList $ blocksToInlines s
toInlines _ (MetaInlines s) = fromList s
toInlines _ (MetaString s) = text s
toInlines n x = unexpectedError "inlines" n x

toBool :: T.Text -> MetaValue -> Bool
toBool _ (MetaBool b) = b
toBool n x = unexpectedError "bool" n x

toBlocks :: T.Text -> MetaValue -> Blocks
toBlocks _ (MetaBlocks bs) = fromList bs
toBlocks _ (MetaInlines ils) = fromList [Plain ils]
toBlocks _ (MetaString s) = plain $ text s
toBlocks n x = unexpectedError "blocks" n x

toString :: T.Text -> MetaValue -> T.Text
toString n x = fromMaybe (unexpectedError "string" n x) $ toMaybeString x

toMaybeString :: MetaValue -> Maybe T.Text
toMaybeString (MetaString s) = Just s
toMaybeString (MetaBlocks b) = Just $ stringify b
toMaybeString (MetaInlines i) = Just $ stringify i
toMaybeString _ = Nothing

getList :: Int -> MetaValue -> Maybe MetaValue
getList i (MetaList l) = l !!? i
  where
    list !!? index | index >= 0 && index < length list = Just $ list !! index
                   | not $ null list = Just $ last list
                   | otherwise = Nothing
getList _ x = Just x

getObj :: T.Text -> MetaValue -> Maybe MetaValue
getObj i (MetaMap m) = M.lookup i m
getObj _ _ = Nothing

capitalize :: (T.Text -> Maybe MetaValue) -> T.Text -> Maybe MetaValue
capitalize f varname = case f (capitalizeFirst varname) of
  Nothing -> case f varname of
    Nothing -> Nothing
    Just x -> Just $ cap x
  Just xs -> Just xs
  where
  cap (MetaString s)  = MetaString $ capitalizeFirst s
  cap (MetaInlines i) = MetaInlines $ walk capStrFst i
  cap (MetaBlocks b)  = MetaBlocks $ walk capStrFst b
  cap (MetaMap m)     = MetaMap $ M.map cap m
  cap (MetaList l)    = MetaList $ map cap l
  cap x               = x
  capStrFst (Str s) = Str $ capitalizeFirst s
  capStrFst x = x
