module Util.Options where

import Text.Pandoc.Definition
import Util.Meta
import Util.Template

data Options = Options { useCleveref :: Bool
                       , sepChapters :: Bool
                       , useListings :: Bool
                       , cbCaptions  :: Bool
                       , figPrefix   :: Int -> [Inline]
                       , eqnPrefix   :: Int -> [Inline]
                       , tblPrefix   :: Int -> [Inline]
                       , lstPrefix   :: Int -> [Inline]
                       , chapDelim   :: [Inline]
                       , rangeDelim  :: [Inline]
                       , lofTitle    :: [Block]
                       , lotTitle    :: [Block]
                       , lolTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       , figureTemplate :: Template
                       , tableTemplate  :: Template
                       , listingTemplate :: Template
                       }

getOptions :: Meta -> Maybe Format -> Options
getOptions dtv fmt =
  Options {
      useCleveref = getMetaBool "cref" dtv
    , sepChapters = getMetaBool "chapters" dtv
    , useListings = getMetaBool "listings" dtv
    , cbCaptions  = getMetaBool "codeBlockCaptions" dtv
    , figPrefix   = getMetaList toInlines "figPrefix" dtv
    , eqnPrefix   = getMetaList toInlines "eqnPrefix" dtv
    , tblPrefix   = getMetaList toInlines "tblPrefix" dtv
    , lstPrefix   = getMetaList toInlines "lstPrefix" dtv
    , chapDelim   = getMetaInlines "chapDelim" dtv
    , rangeDelim  = getMetaInlines "rangeDelim" dtv
    , lofTitle    = getMetaBlock "lofTitle" dtv
    , lotTitle    = getMetaBlock "lotTitle" dtv
    , lolTitle    = getMetaBlock "lolTitle" dtv
    , outFormat   = fmt
    , figureTemplate = makeTemplate dtv $ getMetaInlines "figureTemplate" dtv
    , tableTemplate  = makeTemplate dtv $ getMetaInlines "tableTemplate" dtv
    , listingTemplate = makeTemplate dtv $ getMetaInlines "listingTemplate" dtv
  }
