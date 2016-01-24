{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Text.Pandoc.CrossRef.Util.Settings.Gen where

import Text.Pandoc.CrossRef.Util.Settings.Template
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.Options as O (Options(..))
import Language.Haskell.TH (mkName)
import Text.Pandoc.Definition

nameDeriveSetters ''Options

fmap concat $ mapM (makeAcc . mkName)
  [ "figureTitle"
  , "tableTitle"
  , "listingTitle"
  , "titleDelim"
  , "crossrefYaml"
  , "subfigLabels"
  , "chapters"
  , "figLabels"
  , "eqnLabels"
  , "tblLabels"
  , "lstLabels"
  , "secLabels"
  ]

getOptions :: Meta -> Maybe Format -> Options
getOptions dtv fmt =
  let opts = $(makeCon ''Options 'Options)
  in if getMetaBool "chapters" dtv
     then opts
     else opts{O.chaptersDepth = 0}
