module Util.Default.Settings where

import Text.Pandoc
import Control.Exception (handle,IOException)

import Util.Meta (getMetaString)

getDefaultSettings :: Meta -> IO Meta
getDefaultSettings meta = do
  let handler :: IOException -> IO Meta
      handler _ = return nullMeta
  handle handler $ do
    yaml <- readFile (getMetaString "crossrefYaml" meta nullMeta)
    let Pandoc dtve _ = either (error . show) id $ readMarkdown def ("---\n" ++ yaml ++ "\n---")
    return dtve
