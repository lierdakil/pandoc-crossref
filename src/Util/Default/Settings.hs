module Util.Default.Settings where

import Text.Pandoc
import Control.Exception (handle,IOException)

import Util.Meta (getMetaString)

getDefaultSettings :: Meta -> IO Meta
getDefaultSettings meta = do
  let handler :: IOException -> IO Pandoc
      handler _ = return $ Pandoc nullMeta []
  Pandoc dtve _ <- handle handler $ readMarkdown def `fmap` readFile (getMetaString "crossrefYaml" meta nullMeta)
  return dtve
