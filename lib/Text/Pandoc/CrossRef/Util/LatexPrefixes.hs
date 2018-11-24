module Text.Pandoc.CrossRef.Util.LatexPrefixes where

import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.Definition

data LatexPrefixes = LatexPrefixes {
    latexFigurePrefix :: !String
  , latexTablePrefix :: !String
  , latexEquationPrefix :: !String
  , latexListingPrefix :: !String
  , latexSectionPrefix :: !String
  }

getLatexPrefixes :: String -> Settings -> LatexPrefixes
getLatexPrefixes varN dtv
  | Just (MetaMap kv') <- lookupSettings varN dtv =
    let kv = Settings (Meta kv')
    in LatexPrefixes {
      latexFigurePrefix = getMetaString "figure" kv
    , latexTablePrefix = getMetaString "table" kv
    , latexEquationPrefix = getMetaString "equation" kv
    , latexListingPrefix = getMetaString "listing" kv
    , latexSectionPrefix = getMetaString "section" kv
    }
  | otherwise = error "Prefixes not defined"
