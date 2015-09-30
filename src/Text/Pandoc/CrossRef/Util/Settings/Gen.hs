{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.CrossRef.Util.Settings.Gen where

import Text.Pandoc.CrossRef.Util.Settings.Template

concat `fmap` mapM genSetting
  [ "figureTitle"
  , "tableTitle"
  , "listingTitle"
  , "titleDelim"
  , "chapDelim"
  , "rangeDelim"
  , "figPrefix"
  , "eqnPrefix"
  , "tblPrefix"
  , "lstPrefix"
  , "secPrefix"
  , "lofTitle"
  , "lotTitle"
  , "lolTitle"
  , "figureTemplate"
  , "tableTemplate"
  , "listingTemplate"
  , "crossrefYaml"
  , "chaptersDepth"
  ]
