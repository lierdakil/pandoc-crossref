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

{- |
   Module      : Text.Pandoc.CrossRef.Internal
   Copyright   : Copyright (C) 2025 Nikolay Yakimov
   License     : GNU GPL, version 2 or above

   Maintainer  : Nikolay Yakimov <root@livid.pp.ru>
   Stability   : alpha
   Portability : portable

Internal definitions, exported only for convenience. No stability guarantees.

-}

module Text.Pandoc.CrossRef.Internal (CrossRefEnv(..), CrossRefM(..)) where

import Control.Monad.Reader
import Text.Pandoc

import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References
import Text.Pandoc.CrossRef.Util.Options

-- | Enviromnent for 'CrossRefM'
data CrossRefEnv = CrossRefEnv {
                      creSettings :: Meta -- ^Metadata settings
                    , creOptions :: Options -- ^Internal pandoc-crossref options
                    , creReferences :: References -- ^ Internal state tracking references
                   }

-- | Bit of a weird self-recursive State monad (not MonadFix). The inner reader
-- (function) monad takes the final state (or part of it anyway) as its input.
-- This is all carefully choreographed to lazily converge to a fixpoint, but it
-- is also feasible to evaluate this in two passes instead.
newtype CrossRefM a = CrossRefM (ReaderT Meta WS a)
  deriving (Functor, Applicative, Monad, MonadReader Meta)
