{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright 2009–2014 Roel van Dijk
Copyright 2022 Nikolay Yakimov <root@livid.pp.ru>

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

This file contains code covered by the license found in licenses/LICENSE.roman-numerals
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Parsing and pretty printing of Roman numerals.

This module provides functions for parsing and pretty printing Roman
numerals. Because the notation of Roman numerals has varied through
the centuries this package allows for some customisation using a
configuration that is passed to the conversion functions.

Example:

>>> toRoman 1729 :: String
"MDCCXXIX"

>>> fromRoman "MDCCXXIX" :: Maybe Integer
Just 1729

>>> fromRoman "Bla" :: Maybe Integer
Nothing
-}
module Text.Numeral.Roman
  ( -- * Types
    NumeralConfig
  , mkNumConfig

    -- * Pretty printing
  , convertTo

    -- * Parsing
  , convertFrom

    -- * Default Configurations
  , modernRoman

    -- * Utility
  , toRoman
  , fromRoman
  ) where

import Control.Monad (mzero)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Text as T (Text, null, stripPrefix)
import Data.Text.Internal.Builder
import qualified Data.Text.Lazy as TL (toStrict)

-- |A configuration with which the 'convertTo' and 'convertFrom' functions can
-- be parameterized.
data NumeralConfig n = NC
  { -- |Symbol to represent the value 0.
    ncZero  :: T.Text
    -- |A table of symbols and their numerical values. The table must be
    -- ordered in descending order of the value of the symbols. If any symbol
    -- is the empty string then 'convertFrom' will be &#x22a5;.
  , ncTable :: [(T.Text, n)]
  }

-- |Smart constructor for a 'NumeralConfig'.
mkNumConfig :: (Ord n, Num n)
            => T.Text -- ^Symbol for zero
            -> T.Text -- ^Symbol for one
            -> [(T.Text, n)] -- ^ Symbol-value table.
            -> NumeralConfig n
mkNumConfig z o tab =
  NC { ncZero = z, ncTable = sortBy (flip compare `on` snd) ((o, 1) : tab) }

-- |Converts a number to a Roman numeral according to the given configuration.
convertTo :: (Ord n, Num n) => NumeralConfig n -> n -> T.Text
convertTo nc n | n == 0    = ncZero nc
               | otherwise = TL.toStrict $ toLazyText $ go n $ ncTable nc
  where
    go _ [] = error "Roman.convertTo: out of symbols (BUG)"
    go i tab@(~(sym, val) : ts) | i <= 0    = mempty
                                | i >= val  = fromText sym <> go (i - val) tab
                                | otherwise = go i ts


-- |Parses a string as a Roman numeral according to the given
-- configuration. Result is 'Nothing' if the input is not a valid numeral.
convertFrom :: (Ord n, Num n) => NumeralConfig n -> T.Text -> Maybe n
convertFrom nc s
  | ncZero nc == s = return 0
  | otherwise = do
    n <- go 0 (ncTable nc) s
    if s == convertTo nc n then return n else mzero
  where
    go n _ x | T.null x = return n
    go _ [] _           = mzero
    go n tab@((sym, val) : ts) x =
      maybe (go n ts x) (go (n + val) tab) $ T.stripPrefix sym x

-- |Configuration for Roman numerals as they are commonly used today. The value
-- 0 is represented by the empty string. It can be interpreted as not writing
-- down a number. This configuration is practically limited to the range
-- [1..3999]. Smaller numbers will result in an empty string. Larger numbers
-- will result in repeated use of the \'M\' symbol.
modernRoman :: (Ord n, Num n) => NumeralConfig n
modernRoman = mkNumConfig
  ""
  "I"
  [ ("IV", 4)
  , ("V" , 5)
  , ("IX", 9)
  , ("X" , 10)
  , ("XL", 40)
  , ("L" , 50)
  , ("XC", 90)
  , ("C" , 100)
  , ("CD", 400)
  , ("D" , 500)
  , ("CM", 900)
  , ("M" , 1000)
  ]

-- |Converts a number to a modern Roman numeral.
toRoman :: (Ord n, Num n) => n -> T.Text
toRoman = convertTo modernRoman

-- |Parses a string as a modern Roman numeral.
fromRoman :: (Ord n, Num n) => T.Text -> Maybe n
fromRoman = convertFrom modernRoman
