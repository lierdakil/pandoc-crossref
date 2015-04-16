module Util.Options where

import Text.Pandoc.Definition
import References.Types
import Util.Meta
import Util.Template

data Options = Options { useCleveref :: Bool
                       , sepChapters :: Bool
                       , figPrefix   :: [Inline]
                       , eqnPrefix   :: [Inline]
                       , tblPrefix   :: [Inline]
                       , chapDelim   :: [Inline]
                       , rangeDelim  :: [Inline]
                       , lofTitle    :: [Block]
                       , lotTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       , figureTemplate :: WS [Inline]
                       , tableTemplate  :: WS [Inline]
                       }

getOptions :: Meta -> Maybe Format -> Options
getOptions dtv fmt =
  Options {
      useCleveref = getMetaBool "cref" dtv
    , sepChapters = getMetaBool "chapters" dtv
    , figPrefix   = getMetaInlines "figPrefix" dtv
    , eqnPrefix   = getMetaInlines "eqnPrefix" dtv
    , tblPrefix   = getMetaInlines "tblPrefix" dtv
    , chapDelim   = getMetaInlines "chapDelim" dtv
    , rangeDelim  = getMetaInlines "rangeDelim" dtv
    , lofTitle    = getMetaBlock "lofTitle" dtv
    , lotTitle    = getMetaBlock "lotTitle" dtv
    , outFormat   = fmt
    , figureTemplate = replaceTemplate $ getMetaInlines "figureTemplate" dtv
    , tableTemplate  = replaceTemplate $ getMetaInlines "tableTemplate" dtv
  }
