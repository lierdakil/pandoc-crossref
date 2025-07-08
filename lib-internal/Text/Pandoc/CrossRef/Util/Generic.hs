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

{-# LANGUAGE MonoLocalBinds, TypeAbstractions #-}

module Text.Pandoc.CrossRef.Util.Generic
  ( module Data.Generics
  , runReplace
  , replaceRecurse
  , replaceNoRecurse
  , ReplacedResult
  , noReplaceRecurse
  , mkRR
  , extRR
  , replaceList
  , fixRefs
  , fixRefs'
  , replaceRefsWalk
  ) where

import Data.Generics
import Text.Pandoc.Builder hiding ((<>))
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.References.Refs
import Text.Pandoc.CrossRef.Util.Options
import Lens.Micro.Mtl
import Lens.Micro
import qualified Text.Pandoc.Builder as B

data ReplacedResult a where
  Replaced :: Recurse -> a -> ReplacedResult a
  ReplacedList :: FixRefs a b => ([a] -> WS [a]) -> b -> b -> ReplacedResult [a]
  NotReplaced :: ReplacedResult a
data Recurse = Recurse | NoRecurse
newtype RR m a = RR {unRR :: a -> m (ReplacedResult a)}

class Monoid b => FixRefs a b where
  lens' :: Lens' b [a]

instance FixRefs Inline Inlines where
  lens' = lens B.toList (const B.fromList)

instance FixRefs Block Blocks where
  lens' = lens B.toList (const B.fromList)

instance FixRefs a [a] where
  lens' = id

fixRefs :: (Data a, FixRefs a b, Monad m) => m (Maybe b) -> [a] -> m (ReplacedResult [a])
fixRefs x rest =
  -- NB: must be very-very careful to not inspect the result beyond Maybe,
  -- otherwise fix in deferred part will loop
  x >>= \case
    Just res' -> replaceList' pure res' rest
    Nothing -> noReplaceRecurse

fixRefs' :: Inline -> [Inline] -> WS (ReplacedResult [Inline])
fixRefs' = fixRefs . replaceRefsInline

replaceRefsInline :: Inline -> WS (Maybe Inlines)
replaceRefsInline (Cite cits _) = Just <$> do
  opts <- use wsOptions
  -- recurse into prefix/suffix
  cits' <- doReplaceRefs cits
  liftF $ replaceRefs cits' opts
replaceRefsInline _ = pure Nothing

replaceRefsWalk :: Data a => Options -> References -> a -> a
replaceRefsWalk o r x = fst $ unwrapWS (doReplaceRefs x) (WState o r) r

doReplaceRefs :: Data a => a -> WS a
doReplaceRefs = runReplace $ mkRR \case
  (x:xs) -> fixRefs' x xs
  [] -> noReplaceRecurse

runReplace
  :: Data a
  => (forall d. Data d => (d -> WS (ReplacedResult d)))
  -> a -> WS a
runReplace f x = f x >>= \case
  Replaced Recurse x' -> gmapM (runReplace f) x'
  Replaced NoRecurse x' -> pure x'
  NotReplaced -> gmapM (runReplace f) x
  ReplacedList @a hdT hd tl -> do
    hd' <- hdT `lens'` hd
    tl' <- runReplace @[a] f `lens'` tl
    pure $ view lens' $ hd' <> tl'

mkRR :: (Monad m, Typeable a, Typeable b)
     => (b -> m (ReplacedResult b))
     -> (a -> m (ReplacedResult a))
mkRR = extRR (const noReplaceRecurse)

extRR :: ( Monad m, Typeable a, Typeable b)
     => (a -> m (ReplacedResult a))
     -> (b -> m (ReplacedResult b))
     -> (a -> m (ReplacedResult a))
extRR def' ext = unRR (RR def' `ext0` RR ext)

replaceRecurse :: Monad m => a -> m (ReplacedResult a)
replaceRecurse = pure . Replaced Recurse

replaceNoRecurse :: Data a => a -> WS (ReplacedResult a)
replaceNoRecurse = fmap (Replaced NoRecurse) . doReplaceRefs

replaceList :: (FixRefs a b, Monad m, Monoid b, Data a) => b -> [a] -> m (ReplacedResult [a])
replaceList = replaceList' doReplaceRefs

replaceList' :: (FixRefs a b, Monad m, Monoid b, Data a) => ([a] -> WS [a]) -> b -> [a] -> m (ReplacedResult [a])
replaceList' hdT hd tl = pure $ ReplacedList hdT hd (mempty & lens' .~ tl)

noReplaceRecurse :: Monad m => m (ReplacedResult a)
noReplaceRecurse = pure NotReplaced
