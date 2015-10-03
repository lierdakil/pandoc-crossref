module Text.Pandoc.CrossRef.Util.Settings.Gen where

import Text.Pandoc.CrossRef.Util.Settings.Template
import Text.Pandoc.Builder

figureTitle :: ToMetaValue a => a -> Meta
figureTitle = template "figureTitle"

tableTitle :: ToMetaValue a => a -> Meta
tableTitle = template "tableTitle"

listingTitle :: ToMetaValue a => a -> Meta
listingTitle = template "listingTitle"

titleDelim :: ToMetaValue a => a -> Meta
titleDelim = template "titleDelim"

chapDelim :: ToMetaValue a => a -> Meta
chapDelim = template "chapDelim"

rangeDelim :: ToMetaValue a => a -> Meta
rangeDelim = template "rangeDelim"

figPrefix :: ToMetaValue a => a -> Meta
figPrefix = template "figPrefix"

eqnPrefix :: ToMetaValue a => a -> Meta
eqnPrefix = template "eqnPrefix"

tblPrefix :: ToMetaValue a => a -> Meta
tblPrefix = template "tblPrefix"

lstPrefix :: ToMetaValue a => a -> Meta
lstPrefix = template "lstPrefix"

secPrefix :: ToMetaValue a => a -> Meta
secPrefix = template "secPrefix"

lofTitle :: ToMetaValue a => a -> Meta
lofTitle = template "lofTitle"

lotTitle :: ToMetaValue a => a -> Meta
lotTitle = template "lotTitle"

lolTitle :: ToMetaValue a => a -> Meta
lolTitle = template "lolTitle"

figureTemplate :: ToMetaValue a => a -> Meta
figureTemplate = template "figureTemplate"

tableTemplate :: ToMetaValue a => a -> Meta
tableTemplate = template "tableTemplate"

listingTemplate :: ToMetaValue a => a -> Meta
listingTemplate = template "listingTemplate"

crossrefYaml :: ToMetaValue a => a -> Meta
crossrefYaml = template "crossrefYaml"

chaptersDepth :: ToMetaValue a => a -> Meta
chaptersDepth = template "chaptersDepth"
