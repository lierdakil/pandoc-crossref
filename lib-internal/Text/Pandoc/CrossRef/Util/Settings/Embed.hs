{-# LANGUAGE TemplateHaskell, DeriveLift, StandaloneDeriving #-}
module Text.Pandoc.CrossRef.Util.Settings.Embed where

import Text.Pandoc.CrossRef.Util.Settings.Util
import Text.Pandoc.CrossRef.Util.Settings.LiftPandoc()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath

embedFile :: FilePath -> Q Exp
embedFile name =
  qAddDependentFile fp >>
  runIO (readConfig fp) >>= \v -> [|v|]
  where fp = "settings" </> (name <> ".yaml")
