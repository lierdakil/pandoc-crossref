module Text.Pandoc.CrossRef (
    getSettings
  , crossRefBlocks
  , crossRefMeta
  , runCrossRef
  , runCrossRefIO
  , module SG
  ) where

import Control.Monad.State
import qualified Control.Monad.Reader as R
import Text.Pandoc
import Text.Pandoc.Walk
import Data.Monoid ((<>))

import Text.Pandoc.CrossRef.References
import Text.Pandoc.CrossRef.Util.Settings
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.CodeBlockCaptions
import Text.Pandoc.CrossRef.Util.ModifyMeta
import Text.Pandoc.CrossRef.Util.Settings.Gen as SG

data CrossRefEnv = CrossRefEnv {
                      creSettings :: Meta
                    , creOptions :: Options
                   }

type CrossRefM a = R.Reader CrossRefEnv a

crossRefBlocks :: [Block] -> CrossRefM [Block]
crossRefBlocks blocks = do
  opts <- R.asks creOptions
  let
    doWalk =
      bottomUpM (codeBlockCaptions opts) (walk divBlocks blocks)
      >>= walkM (replaceBlocks opts)
      >>= bottomUpM (replaceRefs opts)
      >>= bottomUpM (listOf opts)
  return $ evalState doWalk def

crossRefMeta :: CrossRefM Meta
crossRefMeta = do
  opts <- R.asks creOptions
  dtv <- R.asks creSettings
  return $ modifyMeta opts dtv

runCrossRef :: (Walkable a b) => Meta -> Maybe Format -> (a -> CrossRefM b) -> a -> b
runCrossRef meta fmt action arg = R.runReader (action arg) env
  where
    settings = meta <> defaultMeta
    env = CrossRefEnv {
            creSettings = settings
          , creOptions = getOptions settings fmt
         }

runCrossRefIO :: (Walkable a b) => Meta -> Maybe Format -> (a -> CrossRefM b) -> a -> IO b
runCrossRefIO meta fmt action arg = do
  settings <- getSettings meta
  let
    env = CrossRefEnv {
            creSettings = settings
          , creOptions = getOptions settings fmt
         }
  return $ R.runReader (action arg) env
