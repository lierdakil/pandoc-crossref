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
>           bs' <- crossRefBlocks bs
>           meta' <- crossRefMeta
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

module Text.Pandoc.CrossRef (
    crossRefBlocks
  , crossRefMeta
  , defaultCrossRefAction
  , runCrossRef
  , runCrossRefIO
  , module SG
  , defaultMeta
  , CrossRefM
  , CrossRefEnv(..)
  ) where

import Lens.Micro.Mtl
import Control.Monad.Reader
import Text.Pandoc

import Text.Pandoc.CrossRef.References
import Text.Pandoc.CrossRef.References.Monad
import Text.Pandoc.CrossRef.Util.ModifyMeta
import Text.Pandoc.CrossRef.Util.Settings
import Text.Pandoc.CrossRef.Util.Settings.Gen as SG

import Text.Pandoc.CrossRef.Internal

{- | Walk over blocks, while inserting cross-references, list-of, etc.

Works in 'CrossRefM' monad. -}
crossRefBlocks :: [Block] -> CrossRefM [Block]
crossRefBlocks = CrossRefM . lift . replaceAll

{- | Modifies metadata, adding header-includes instructions to setup custom and
builtin environments, plus list-of-x metadata fields if
'Text.Pandoc.CrossRef.Util.Settings.Gen.listOfMetadata' is enabled.

Works in 'CrossRefM' monad. -}
crossRefMeta :: CrossRefM Meta
crossRefMeta = CrossRefM do
  settings <- ask
  lift do
    opts <- use wsOptions
    liftF $ modifyMeta opts settings

{- | Combines 'crossRefMeta' and 'crossRefBlocks'

Works in 'CrossRefM' monad. -}
defaultCrossRefAction :: Pandoc -> CrossRefM Pandoc
defaultCrossRefAction (Pandoc _ bs) = Pandoc <$> crossRefMeta <*> crossRefBlocks bs

{- | Run an action in 'CrossRefM' monad with argument, and return pure result.

This is the primary function to work with 'CrossRefM' -}
runCrossRef :: forall a b. Meta -> Maybe Format -> (a -> CrossRefM b) -> a -> b
runCrossRef meta fmt action arg = runCrossRefInternal env $ action arg
  where
    settings = defaultMeta <> meta
    env = CrossRefEnv {
            creSettings = settings
          , creOptions = getOptions settings fmt
          , creReferences = def
         }

{- | Run an action in 'CrossRefM' monad with argument, and return 'IO' result.

This function will attempt to read pandoc-crossref settings from settings
file specified by crossrefYaml metadata field. -}
runCrossRefIO :: forall a b. Meta -> Maybe Format -> (a -> CrossRefM b) -> a -> IO b
runCrossRefIO meta fmt action arg = do
  settings <- getSettings fmt meta
  let
    env = CrossRefEnv {
            creSettings = settings
          , creOptions = getOptions settings fmt
          , creReferences = def
         }
  pure $ runCrossRefInternal env $ action arg

runCrossRefInternal :: CrossRefEnv -> CrossRefM b -> b
runCrossRefInternal CrossRefEnv { creOptions, creReferences, creSettings } (CrossRefM action) =
  fst $ runWS (WState creOptions creReferences) (runReaderT action creSettings)
