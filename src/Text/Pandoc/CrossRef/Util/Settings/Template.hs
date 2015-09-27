{-# LANGUAGE QuasiQuotes, TemplateHaskell, RankNTypes, FlexibleInstances #-}
module Text.Pandoc.CrossRef.Util.Settings.Template where

import Language.Haskell.TH
import Text.Pandoc
import Text.Pandoc.Builder

genSetting :: String -> Q [Dec]
genSetting name = do
  sig <- [t| forall a. ToMetaValue a => a -> Meta |]
  return
    [ SigD func sig
    , FunD func [Clause [VarP value] (NormalB
        (AppE (ConE $ mkName "Meta")
          (AppE (AppE (VarE (mkName "M.singleton")) (LitE (StringL name)))
            (AppE (VarE (mkName "toMetaValue")) (VarE value))))
      ) []]]
  where
    func = mkName name
    value = mkName "value"
