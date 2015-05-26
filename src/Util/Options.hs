{-# LANGUAGE FlexibleContexts #-}

module Util.Options (Options(..), getOptions) where
import Text.Pandoc.Definition
import Util.Meta
import Util.Template
import Util.Util (capitalizeFirst)
import Text.Pandoc.Walk
import Data.Default
-- import Control.Monad.Identity

data Options = Options { useCleveref :: Bool
                       , sepChapters :: Bool
                       , useListings :: Bool
                       , cbCaptions  :: Bool
                       , figPrefix   :: Bool -> Int -> [Inline]
                       , eqnPrefix   :: Bool -> Int -> [Inline]
                       , tblPrefix   :: Bool -> Int -> [Inline]
                       , lstPrefix   :: Bool -> Int -> [Inline]
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
    , figPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "figPrefix"
    , eqnPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "eqnPrefix"
    , tblPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "tblPrefix"
    , lstPrefix   = tryCapitalizeM (flip (getMetaList toInlines) dtv) "lstPrefix"
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

tryCapitalizeM :: (Functor m, Monad m, Walkable Inline a, Default a, Eq a) =>
        (String -> m a) -> String -> Bool -> m a
tryCapitalizeM f varname capitalize
  | capitalize = do
    res <- f (capitalizeFirst varname)
    case res of
      xs | xs == def -> f varname >>= walkM capStrFst
         | otherwise -> return xs
  | otherwise  = f varname
  where
    capStrFst (Str s) = return $ Str $ capitalizeFirst s
    capStrFst x = return x

-- tryCapitalize :: (Walkable Inline a, Default a, Eq a) =>
--         (String -> a) -> String -> Bool -> a
-- tryCapitalize = ((runIdentity .) .) . tryCapitalizeM . (return .)
