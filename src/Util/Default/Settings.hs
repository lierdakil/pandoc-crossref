module Util.Default.Settings where

import Text.Pandoc.Definition
import qualified Data.Map as M
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
import Control.Exception (handle,IOException)

import Util.Default.Types
import Util.Meta (getMetaString)

getDefaultSettings :: Meta -> IO DefaultSettings
getDefaultSettings meta = do
  let handler :: IOException -> IO (Either String (M.Map String String))
      handler _ = return $ Right M.empty
  dtve <-handle handler $ Y.decodeEither `fmap` B.readFile (getMetaString "crossrefYaml" meta M.empty)
  case dtve of
      Right dtv' -> return dtv'
      Left e -> error e
