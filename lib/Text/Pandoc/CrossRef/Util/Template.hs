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

{-# LANGUAGE RecordWildCards #-}

module Text.Pandoc.CrossRef.Util.Template
  ( Template
  , RefTemplate
  , makeTemplate
  , makeRefTemplate
  , applyTemplate
  , applyRefTemplate
  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Generic
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.Settings.Types
import Control.Applicative
import Text.Read
import Data.Char (isAlphaNum, isUpper, toLower)
import Control.Monad (join)

type VarFunc = String -> Maybe MetaValue
newtype Template = Template (VarFunc -> Inlines)
newtype RefTemplate = RefTemplate (Bool -> Int -> Inlines)

data State = StFirstVar | StIndex | StAfterIndex | StPrefix | StSuffix
data ParseRes = ParseRes { prVar :: String, prIdx :: Maybe String, prPfx :: String, prSfx :: String } deriving Show

parse :: State -> String -> ParseRes
parse _ [] = ParseRes [] Nothing [] []
parse StFirstVar cs@(c:_) | isAlphaNum c = let (var, rest) = span isAlphaNum cs in (parse StFirstVar rest){prVar = var}
parse StFirstVar ('[':cs) = let (idx, rest) = span isAlphaNum cs in (parse StIndex rest){prIdx = Just idx}
parse StIndex (']':cs) = parse StAfterIndex cs
parse StIndex _ = error "Unterminated [ in indexed variable"
parse _ ('%':cs) = parse StSuffix cs
parse _ ('#':cs) = parse StPrefix cs
parse StFirstVar s = error $ "Invalid variable name in " <> s
parse StAfterIndex (c:_) = error $ "Unexpected character " <> [c] <> " after parsing indexed variable"
parse StPrefix cs = let (pfx, rest) = span (`notElem` "%#") cs in (parse StPrefix rest){prPfx = pfx}
parse StSuffix cs = let (sfx, rest) = span (`notElem` "%#") cs in (parse StSuffix rest){prSfx = sfx}

makeTemplate :: Settings -> Inlines -> Template
makeTemplate dtv xs' = Template $ \vf -> fromList $ scan (\var -> vf var <|> lookupSettings var dtv) $ toList xs'
  where
  scan :: (String -> Maybe MetaValue) -> [Inline] -> [Inline]
  scan = bottomUp . go
  go vf (Math DisplayMath var:xs)
    | ParseRes{..} <- parse StFirstVar var
    = let replaceVar = maybe mempty (modifier . toInlines ("variable " ++ var))
          modifier = (<> text prSfx) . (text prPfx <>)
      in case prIdx of
        Just idxVar ->
          let
            idx = readMaybe . toString ("index variable " ++ idxVar) =<< vf idxVar
            arr = join $ getList <$> idx <*> vf prVar
          in toList $ replaceVar arr <> fromList xs
        Nothing -> toList $ replaceVar (vf prVar) <> fromList xs
  go _ (x:xs) = toList $ singleton x <> fromList xs
  go _ [] = []

makeRefTemplate :: Settings -> Inlines -> RefTemplate
makeRefTemplate dtv xs' =
  let Template g = makeTemplate dtv xs'
      vf _ n "n" = Just $ MetaInlines [Str $ show n]
      vf cap _ (vc:vs)
        | isUpper vc && cap = capitalize (`lookupSettings` dtv) var
        | otherwise = lookupSettings var dtv
        where var = toLower vc : vs
      vf _ _ [] = error "Empty variable name"
  in RefTemplate $ \cap n -> g (vf cap n)

applyRefTemplate :: RefTemplate -> Bool -> Int -> Inlines
applyRefTemplate (RefTemplate g) = g

applyTemplate :: (String -> Maybe Inlines) -> Template -> Inlines
applyTemplate vars (Template g) = g $ fmap (MetaInlines . toList) . vars
