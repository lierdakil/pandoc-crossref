module Util.Default.Settings where

import Text.Pandoc
import Control.Exception (handle,IOException)

import Util.Meta (getMetaString)

getDefaultSettings :: Meta -> IO Meta
getDefaultSettings meta = do
  let handler :: IOException -> IO String
      handler _ = return []
  yaml <- handle handler $ readFile (getMetaString "crossrefYaml" meta nullMeta)
  let Pandoc dtve _ = readMarkdown def ("---\n" ++ yaml ++ "\n---")
  return dtve
