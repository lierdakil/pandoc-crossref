{-# LANGUAGE TemplateHaskellQuotes #-}
module ManData where

import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import System.IO
import qualified Text.Pandoc as P
import Control.DeepSeq
import Data.String
import Text.Pandoc.Highlighting (pygments)

dataFile :: FilePath
dataFile = "docs/index.md"

readDataFile :: IO String
readDataFile =
  withFile dataFile ReadMode $ \h -> do
    hSetEncoding h utf8
    cont <- hGetContents h
    return $!! cont

embedManual :: (P.Pandoc -> P.PandocPure T.Text) -> Q Exp
embedManual fmt = do
  qAddDependentFile dataFile
  d <- runIO readDataFile
  let pd = either (error . show) id $ P.runPure $ P.readMarkdown readerOpts (T.pack d)
  let txt = either (error . show) id $ P.runPure $ fmt pd
  strToExp $ T.unpack txt

readerOpts :: P.ReaderOptions
readerOpts = P.def{
    P.readerExtensions = P.enableExtension P.Ext_yaml_metadata_block P.githubMarkdownExtensions
  , P.readerStandalone = True
}

embedManualText :: Q Exp
embedManualText = embedManual $ P.writePlain P.def

embedManualHtml :: Q Exp
embedManualHtml = do
  t <- runIO $ fmap (either (error . show) id) $ P.runIO $ P.getDefaultTemplate "html5"
  embedManual $ P.writeHtml5String P.def{
    P.writerTemplate = Just t
  , P.writerHighlightStyle = Just pygments
  }

strToExp :: String -> Q Exp
strToExp s =
    return $ VarE 'fromString
      `AppE` LitE (StringL s)
