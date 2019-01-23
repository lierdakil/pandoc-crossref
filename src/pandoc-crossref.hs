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

{-# LANGUAGE ApplicativeDo, TemplateHaskell, CPP, OverloadedStrings #-}
import Text.Pandoc
import Text.Pandoc.JSON

import Text.Pandoc.CrossRef
import Options.Applicative
import qualified Options.Applicative as O
import Control.Monad
import Web.Browser
import System.IO.Temp
import System.IO hiding (putStrLn)
import ManData
import Control.Concurrent
import Development.GitRev
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (putStrLn)

man, manHtml :: T.Text
man = T.pack $(embedManualText)
manHtml = T.pack $(embedManualHtml)

data Flag = NumericVersion | Version | Man | HtmlMan

run :: Parser (IO ())
run = do
  numVers <- flag Nothing (Just NumericVersion) (long "numeric-version" <> help "Print version")
  vers <- flag Nothing (Just Version) (long "version" <> short 'v' <> help "Print version")
  man' <- flag Nothing (Just Man) (long "man" <> help "Show manpage")
  hman <- flag Nothing (Just HtmlMan) (long "man-html" <> help "Show html manpage")
  fmt <- optional $ strArgument (metavar "FORMAT")
  return $ go (numVers <|> vers <|> man' <|> hman) fmt
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
    go Nothing _ = toJSONFilter f
    f fmt p@(Pandoc meta _) =
      runCrossRefIO meta fmt defaultCrossRefAction p

main :: IO ()
main = join $ execParser opts
  where
    opts = info (run <**> helper)
      (  fullDesc
      <> O.header "pandoc-crossref - Pandoc filter for cross-references"
      )
