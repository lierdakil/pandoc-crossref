{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Text.Pandoc.CrossRef.Util.Settings.Template where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import qualified Data.Map as M

genSetting :: String -> Q [Dec]
genSetting name = sequenceQ
  [ sigD func [t| forall a. ToMetaValue a => a -> Meta |]
  , valD (varP func) (normalB [e| Meta . M.singleton $(lift name) . toMetaValue |]) []
  ] where func = mkName name
