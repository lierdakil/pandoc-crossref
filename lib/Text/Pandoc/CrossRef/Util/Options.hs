module Text.Pandoc.CrossRef.Util.Options (Options(..)) where
import Text.Pandoc.Definition
import Text.Pandoc.CrossRef.Util.Template

data Options = Options { cref :: Bool
                       , chaptersDepth   :: Int
                       , listings :: Bool
                       , codeBlockCaptions  :: Bool
                       , autoSectionLabels  :: Bool
                       , figPrefix   :: Bool -> Int -> [Inline]
                       , eqnPrefix   :: Bool -> Int -> [Inline]
                       , tblPrefix   :: Bool -> Int -> [Inline]
                       , lstPrefix   :: Bool -> Int -> [Inline]
                       , secPrefix   :: Bool -> Int -> [Inline]
                       , figPrefixTemplate :: Template
                       , eqnPrefixTemplate :: Template
                       , tblPrefixTemplate :: Template
                       , lstPrefixTemplate :: Template
                       , secPrefixTemplate :: Template
                       , chapDelim   :: [Inline]
                       , rangeDelim  :: [Inline]
                       , lofTitle    :: [Block]
                       , lotTitle    :: [Block]
                       , lolTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       , figureTemplate :: Template
                       , subfigureTemplate :: Template
                       , subfigureChildTemplate :: Template
                       , ccsTemplate :: Template
                       , tableTemplate  :: Template
                       , listingTemplate :: Template
                       , customLabel :: String -> Int -> Maybe String
                       , ccsDelim :: [Inline]
                       , ccsLabelSep :: [Inline]
                       , tableEqns :: Bool
                       }
