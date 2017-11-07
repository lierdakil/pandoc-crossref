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
import Data.FileEmbed
import Web.Browser
import System.IO.Temp
import System.IO

man :: String
man = $(embedStringFile "docs/man.txt")

manHtml :: String
manHtml = $(embedStringFile "docs/index.html")

data Flag = Version | Man | HtmlMan

run :: Parser (IO ())
run = do
  fmt <- optional $ strOption (
    long "format" <> short 'f' <> metavar "FORMAT" <> help "Force output format"
    )
  vers <- flag Nothing (Just Version) (long "version" <> short 'v' <> help "Print version")
  man' <- flag Nothing (Just Man) (long "man" <> help "Show manpage")
  hman <- flag Nothing (Just HtmlMan) (long "man-html" <> help "Show html manpage")
  return $ go (vers <|> man' <|> hman) fmt
  where
    go (Just Version) _ = putStrLn $ showVersion version
    go (Just Man    ) _ = putStrLn man
    go (Just HtmlMan) _ = withSystemTempFile "pandoc-crossref-manual.html" $ \fp h -> do
      hPutStrLn h manHtml
      void $ openBrowser $ "file:///" <> fp
      return ()
    go Nothing x = toJSONFilter $ f x
    f ffmt fmt p@(Pandoc meta _) =
      runCrossRefIO meta (Format <$> ffmt <|> fmt) defaultCrossRefAction p

main :: IO ()
main = join $ execParser opts
  where
    opts = info (run <**> helper)
      (  fullDesc
      <> O.header "pandoc-crossref - Pandoc filter for cross-references"
      )
