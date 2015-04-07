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

getOptions :: Meta -> Meta -> Maybe Format -> Options
getOptions meta dtv fmt =
  Options {
      useCleveref = getMetaBool "cref" meta dtv
    , sepChapters = getMetaBool "chapters" meta dtv
    , figPrefix   = getMetaInlines "figPrefix" meta dtv
    , eqnPrefix   = getMetaInlines "eqnPrefix" meta dtv
    , tblPrefix   = getMetaInlines "tblPrefix" meta dtv
    , chapDelim   = getMetaInlines "chapDelim" meta dtv
    , rangeDelim  = getMetaInlines "rangeDelim" meta dtv
    , lofTitle    = getMetaBlock "lofTitle" meta dtv
    , lotTitle    = getMetaBlock "lotTitle" meta dtv
    , outFormat   = fmt
    , figureTemplate = replaceTemplate $ getMetaInlines "figureTemplate" meta dtv
    , tableTemplate  = replaceTemplate $ getMetaInlines "tableTemplate" meta dtv
  }
