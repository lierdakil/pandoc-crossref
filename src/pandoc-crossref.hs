{-# LANGUAGE ApplicativeDo #-}
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.JSON

import Text.Pandoc.CrossRef
import Options.Applicative
import qualified Options.Applicative as O
import Control.Monad
import Paths_pandoc_crossref (version)
import Data.Version (showVersion)

run :: Parser (IO ())
run = do
  vers <- switch (long "version" <> short 'v' <> help "Print version")
  fmt <- optional $ strOption (
    long "format" <> short 'f' <> metavar "FORMAT" <> help "Force output format"
    )
  man <- switch (long "man" <> help "Show manpage")
  return $ go vers man fmt
  where
    go True _ _ = putStrLn $ showVersion version
    go _ True _ = putStrLn $ showVersion version
    go _ _ x = toJSONFilter $ f x
    f ffmt fmt p@(Pandoc meta _) =
      runCrossRefIO meta (Format <$> ffmt <|> fmt) defaultCrossRefAction p

main :: IO ()
main = join $ execParser opts
  where
    opts = info (run <**> helper)
      (  fullDesc
      <> O.header "pandoc-crossref - Pandoc filter for cross-references"
      )
