{-# LANGUAGE ApplicativeDo, TemplateHaskell #-}
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.JSON

import Text.Pandoc.CrossRef
import Options.Applicative
import qualified Options.Applicative as O
import Control.Monad
import Paths_pandoc_crossref (version)
import Data.Version (showVersion)
import Web.Browser
import System.IO.Temp
import System.IO
import ManData
import Control.Concurrent

man, manHtml :: String
man = $(embedManualText)
manHtml = $(embedManualHtml)

data Flag = Version | Man | HtmlMan

run :: Parser (IO ())
run = do
  vers <- flag Nothing (Just Version) (long "version" <> short 'v' <> help "Print version")
  man' <- flag Nothing (Just Man) (long "man" <> help "Show manpage")
  hman <- flag Nothing (Just HtmlMan) (long "man-html" <> help "Show html manpage")
  fmt <- optional $ strArgument (metavar "FORMAT")
  return $ go (vers <|> man' <|> hman) fmt
  where
    go :: Maybe Flag -> Maybe String -> IO ()
    go (Just Version) _ = putStrLn $ showVersion version
    go (Just Man    ) _ = putStrLn man
    go (Just HtmlMan) _ = withSystemTempFile "pandoc-crossref-manual.html" $ \fp h -> do
      hPutStrLn h manHtml
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
