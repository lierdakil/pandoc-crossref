{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.CrossRef.Util.Settings.Util where

import Text.Pandoc.CrossRef.Util.Settings.Types
import System.IO
import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Walk

readMd :: T.Text -> IO Pandoc
readMd = handleError . runPure . readMarkdown def{
  readerExtensions=disableExtension Ext_auto_identifiers pandocExtensions
  }

readConfig :: FilePath -> IO Settings
readConfig path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    yaml <- hGetContents h
    Pandoc meta' _ <- readMd $ T.pack $ unlines ["---", yaml, "---"]
    return . normalizeSpaces $ Settings meta'

normalizeSpaces :: Settings -> Settings
normalizeSpaces (Settings s) = Settings $ walk walkInlines s
  where
  walkInlines :: Inline -> Inline
  walkInlines (Span ("",["s"],[]) []) = Space
  walkInlines x = x
