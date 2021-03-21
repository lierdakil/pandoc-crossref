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

{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Text.Pandoc.CrossRef.Util.Replace (
    module Text.Pandoc.CrossRef.Util.Replace
  , module Data.Generics
) where

import Text.Pandoc.CrossRef.References.Types
import Data.Generics hiding (Prefix)

data ReplacedResult a = ReplacedRecurse Scope a
                      | NotReplacedRecurse Scope
                      | ReplacedNoRecurse a
                      | NotReplacedNoRecurse
type GenRR m = forall a. Data a => (Scope -> a -> m (ReplacedResult a))
newtype RR m a = RR {unRR :: Scope -> a -> m (ReplacedResult a)}

runReplace :: (MonadError WSException m, Monad m) => Scope -> GenRR m -> GenericM m
runReplace s f x = do
  res <- f s x `catchError` handler
  case res of
    ReplacedRecurse s' x' -> gmapM (runReplace s' f) x'
    ReplacedNoRecurse x' -> return x'
    NotReplacedRecurse s' -> gmapM (runReplace s' f) x
    NotReplacedNoRecurse -> return x
  where
  handler (WSENoSuchPrefix _) = return $ NotReplacedRecurse s
  handler e = throwError e

mkRR :: (Monad m, Typeable a, Typeable b)
     => (Scope -> b -> m (ReplacedResult b))
     -> (Scope -> a -> m (ReplacedResult a))
mkRR = extRR (\s _ -> noReplaceRecurse s)

extRR :: ( Monad m, Typeable a, Typeable b)
     => (Scope -> a -> m (ReplacedResult a))
     -> (Scope -> b -> m (ReplacedResult b))
     -> (Scope -> a -> m (ReplacedResult a))
extRR def' ext = unRR (RR def' `ext0` RR ext)

replaceRecurse :: Monad m => Scope -> a -> m (ReplacedResult a)
replaceRecurse s = return . ReplacedRecurse s

replaceNoRecurse :: Monad m => a -> m (ReplacedResult a)
replaceNoRecurse = return . ReplacedNoRecurse

noReplaceRecurse :: Monad m => Scope -> m (ReplacedResult a)
noReplaceRecurse = return . NotReplacedRecurse

noReplaceNoRecurse :: Monad m => m (ReplacedResult a)
noReplaceNoRecurse = return NotReplacedNoRecurse
