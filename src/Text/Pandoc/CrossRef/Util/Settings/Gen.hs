{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.CrossRef.Util.Settings.Gen where

import Text.Pandoc
import qualified Data.Map as M

import Text.Pandoc.CrossRef.Util.Settings.Template
import Text.Pandoc.Builder

$(concat `fmap` mapM genSetting
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
  ])
