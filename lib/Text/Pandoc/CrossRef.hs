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
   Module      : Text.Pandoc.CrossRef
   Copyright   : Copyright (C) 2015 Nikolay Yakimov
   License     : GNU GPL, version 2 or above

   Maintainer  : Nikolay Yakimov <root@livid.pp.ru>
   Stability   : alpha
   Portability : portable

Public interface to pandoc-crossref library

Example of use:

> import Text.Pandoc
> import Text.Pandoc.JSON
>
> import Text.Pandoc.CrossRef
>
> main :: IO ()
> main = toJSONFilter go
>   where
>     go fmt p@(Pandoc meta _) = runCrossRefIO meta fmt action p
>       where
>         action (Pandoc _ bs) = do
>           meta' <- crossRefMeta
>           bs' <- crossRefBlocks bs
>           return $ Pandoc meta' bs'

This module also exports utility functions for setting up meta-settings for
pandoc-crossref. Refer to documentation for a complete list of metadata field
names. All functions accept a single argument of type, returned by
"Text.Pandoc.Builder" functions, and return 'Meta'.

Example:

> runCrossRefIO meta fmt crossRefBlocks blocks
>   where
>     meta =
>          figureTitle (str "Figura")
>       <> tableTitle (str "Tabla")
>       <> figPrefix (str "fig.")
>       <> eqnPrefix (str "ec.")
>       <> tblPrefix (str "tbl.")
>       <> loftitle (header 1 $ text "Lista de figuras")
>       <> lotTitle (header 1 $ text "Lista de tablas")
>       <> chaptersDepth (MetaString "2")

-}
{-# LANGUAGE RankNTypes #-}

module Text.Pandoc.CrossRef (
    crossRefBlocks
  , crossRefMeta
  , defaultCrossRefAction
  , runCrossRef
  , runCrossRefIO
  , module SG
  , defaultMeta
  , CrossRef
  , CrossRefEnv(..)
  , WSException(..)
  ) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer as W
import Control.Monad.Reader as R
import Text.Pandoc as P
import Data.Monoid ((<>))
import System.IO

import Text.Pandoc.CrossRef.References
import Text.Pandoc.CrossRef.Util.Settings
import Text.Pandoc.CrossRef.Util.ModifyMeta
import Text.Pandoc.CrossRef.Util.Settings.Gen as SG

{- | Walk over blocks, while inserting cross-references, list-of, etc.

Works in 'CrossRefM' monad. -}
crossRefBlocks :: [Block] -> CrossRef [Block]
crossRefBlocks blocks = CrossRef $ flip evalStateT def . unWS $ doWalk
  where doWalk =
          replaceAll blocks
          >>= bottomUpM replaceRefs
          >>= bottomUpM listOf

{- | Modifies metadata for LaTeX output, adding header-includes instructions
to setup custom and builtin environments.

Note, that if output format is not "latex", this function does nothing.

Works in 'CrossRefM' monad. -}
crossRefMeta :: CrossRef Meta
crossRefMeta = modifyMeta

{- | Combines 'crossRefMeta' and 'crossRefBlocks'

Works in 'CrossRefM' monad. -}
defaultCrossRefAction :: Pandoc -> CrossRef Pandoc
defaultCrossRefAction (Pandoc _ bs) = do
  meta' <- crossRefMeta
  bs' <- crossRefBlocks bs
  return $ Pandoc meta' bs'

{- | Run an action in 'CrossRefM' monad with argument, and return pure result.

This is primary function to work with 'CrossRefM' -}
runCrossRef :: forall b. Meta -> Maybe Format -> CrossRef b -> (Either WSException b, [String])
runCrossRef meta fmt = flip runReader env . runWriterT . runExceptT . unCrossRef
  where
    settings = Settings meta <> defaultMeta
    env = CrossRefEnv {
            creSettings = settings
          , creOptions = getOptions settings fmt
         }

{- | Run an action in 'CrossRefM' monad with argument, and return 'IO' result.

This function will attempt to read pandoc-crossref settings from settings
file specified by crossrefYaml metadata field. -}
runCrossRefIO :: forall b. Meta -> Maybe Format -> CrossRef b -> IO b
runCrossRefIO meta fmt action = do
  Settings meta' <- readSettings fmt meta
  let (res, lg) = runCrossRef meta' fmt action
  mapM_ (hPutStrLn stderr) lg
  return $ either (error . pretty) id res
