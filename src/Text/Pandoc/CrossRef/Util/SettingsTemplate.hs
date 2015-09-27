{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Text.Pandoc.CrossRef.Util.SettingsTemplate where

import Language.Haskell.TH
import Text.Pandoc

genSetting :: String -> Q [Dec]
genSetting name = do
  sig <- [t| MetaValue -> Meta |]
  return
    [ SigD func sig
    , FunD func [Clause [VarP value] (NormalB
        (AppE (ConE $ mkName "Meta") (AppE (AppE (VarE (mkName "M.singleton")) (LitE (StringL name))) (VarE value)))
      ) []]]
  where
    func = mkName name
    value = mkName "value"

genSettings :: [String] -> Q [Dec]
genSettings = concatMapM genSetting
  where
    concatMapM f xs = fmap concat (mapM f xs)
