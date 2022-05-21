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

{-# LANGUAGE ApplicativeDo, TemplateHaskell, CPP, OverloadedStrings,
  LambdaCase #-}
import Text.Pandoc
import Text.Pandoc.JSON

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.GitRev
import ManData
import Options.Applicative
import qualified Options.Applicative as O
import System.Environment as Env
import System.IO hiding (putStrLn)
import System.IO.Temp
import Text.Pandoc.CrossRef
import Web.Browser

import Prelude hiding (putStrLn)

man, manHtml :: T.Text
man = T.pack $(embedManualText)
manHtml = T.pack $(embedManualHtml)

data Flag = NumericVersion | Version | Man | HtmlMan | Pipe

run :: Parser (IO ())
run = do
  numVers <- flag Nothing (Just NumericVersion) (long "numeric-version" <> help "Print version")
  vers <- flag Nothing (Just Version) (long "version" <> short 'v' <> help "Print version")
  man' <- flag Nothing (Just Man) (long "man" <> help "Show manpage")
  hman <- flag Nothing (Just HtmlMan) (long "man-html" <> help "Show html manpage")
  pipe <- flag Nothing (Just Pipe) (short 'p' <> long "pipe" <> help "Run in \"pipe mode\", i.e. read pandoc JSON from stdin and output it to stdout")
  fmt <- optional $ strArgument (metavar "FORMAT")
  return $ go (numVers <|> vers <|> man' <|> hman <|> pipe) fmt
  where
    go :: Maybe Flag -> Maybe String -> IO ()
    go (Just Version) _ = T.putStrLn $
         "pandoc-crossref v" <> VERSION_pandoc_crossref
      <> " git commit " <> $gitHash
      <> " (" <> $gitBranch <> ")"
      <> " built with Pandoc v" <> VERSION_pandoc <> ","
      <> " pandoc-types v" <> VERSION_pandoc_types
      <> " and GHC " <> TOOL_VERSION_ghc
    go (Just NumericVersion) _ = T.putStrLn VERSION_pandoc_crossref
    go (Just Man    ) _ = T.putStrLn man
    go (Just HtmlMan) _ = withSystemTempFile "pandoc-crossref-manual.html" $ \fp h -> do
      hSetEncoding h utf8
      T.hPutStrLn h manHtml
      hClose h
      void $ openBrowser $ "file:///" <> fp
      threadDelay 5000000
      return ()
    go (Just Pipe) fmt = toJSONFilter (f fmt)
    go Nothing Nothing = hPutStr stderr $ unlines
      [ "Running pandoc-crossref without arguments is not supported. Try"
      , "\tpandoc-crossref --help"
      , "If you want to run in \"pipe mode\", run with"
      , "\tpandoc-crossref --pipe [FORMAT]"
      ]
    go Nothing fmt = do
      Env.lookupEnv "PANDOC_VERSION" >>= \case
        Just runv ->
          when (VERSION_pandoc /= runv) $ hPutStrLn stderr $
            "WARNING: pandoc-crossref was compiled with pandoc " <> VERSION_pandoc <>
            " but is being run through " <> runv <> ". This is not supported. " <>
            "Strange things may (and likely will) happen silently."
        Nothing -> hPutStr stderr $ unlines
          [ "WARNING: Running pandoc-crossref in \"pipe mode\" implicitly is deprecated. Please use"
          , "\tpandoc-crossref --pipe [FORMAT]"
          , "instead."
          ]
      toJSONFilter (f fmt)
    f fmt p@(Pandoc meta _) = runCrossRefIO meta (Format . T.pack <$> fmt) defaultCrossRefAction p

main :: IO ()
main = join $ execParser opts
  where
    opts = info (run <**> helper)
      (  fullDesc
      <> O.header "pandoc-crossref - Pandoc filter for cross-references"
      )
