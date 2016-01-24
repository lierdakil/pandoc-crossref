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
  , CrossRefM
  , CrossRefEnv(..)
  ) where

import Control.Monad.State
import qualified Control.Monad.Reader as R
import Text.Pandoc
import Data.Monoid ((<>))

import Text.Pandoc.CrossRef.References
import Text.Pandoc.CrossRef.Util.Settings
import Text.Pandoc.CrossRef.Util.Options as O
import Text.Pandoc.CrossRef.Util.CodeBlockCaptions
import Text.Pandoc.CrossRef.Util.ModifyMeta
import Text.Pandoc.CrossRef.Util.Settings.Gen as SG

-- | Enviromnent for 'CrossRefM'
data CrossRefEnv = CrossRefEnv {
                      creSettings :: Meta -- ^Metadata settings
                    , creOptions :: Options -- ^Internal pandoc-crossref options
                   }

-- | Essentially a reader monad for basic pandoc-crossref environment
type CrossRefM a = R.Reader CrossRefEnv a

{- | Walk over blocks, while inserting cross-references, list-of, etc.

Works in 'CrossRefM' monad. -}
crossRefBlocks :: [Block] -> CrossRefM [Block]
crossRefBlocks blocks = do
  opts <- R.asks creOptions
  let
    doWalk =
      bottomUpM (mkCodeBlockCaptions opts) blocks
      >>= replaceAll opts
      >>= bottomUpM (replaceRefs opts)
      >>= bottomUpM (listOf opts)
  return $ evalState doWalk def

{- | Modifies metadata for LaTeX output, adding header-includes instructions
to setup custom and builtin environments.

Note, that if output format is not "latex", this function does nothing.

Works in 'CrossRefM' monad. -}
crossRefMeta :: CrossRefM Meta
crossRefMeta = do
  opts <- R.asks creOptions
  dtv <- R.asks creSettings
  return $ modifyMeta opts dtv

{- | Combines 'crossRefMeta' and 'crossRefBlocks'

Works in 'CrossRefM' monad. -}
defaultCrossRefAction :: Pandoc -> CrossRefM Pandoc
defaultCrossRefAction (Pandoc _ bs) = do
  meta' <- crossRefMeta
  bs' <- crossRefBlocks bs
  return $ Pandoc meta' bs'

{- | Run an action in 'CrossRefM' monad with argument, and return pure result.

This is primary function to work with 'CrossRefM' -}
runCrossRef :: forall a b. Meta -> Maybe Format -> (a -> CrossRefM b) -> a -> b
runCrossRef meta fmt action arg = R.runReader (action arg) env
  where
    settings = meta <> defaultMeta
    env = CrossRefEnv {
            creSettings = settings
          , creOptions = getOptions settings fmt
         }

{- | Run an action in 'CrossRefM' monad with argument, and return 'IO' result.

This function will attempt to read pandoc-crossref settings from settings
file specified by crossrefYaml metadata field. -}
runCrossRefIO :: forall a b. Meta -> Maybe Format -> (a -> CrossRefM b) -> a -> IO b
runCrossRefIO meta fmt action arg = do
  settings <- getSettings meta
  let
    env = CrossRefEnv {
            creSettings = settings
          , creOptions = getOptions settings fmt
         }
  return $ R.runReader (action arg) env
