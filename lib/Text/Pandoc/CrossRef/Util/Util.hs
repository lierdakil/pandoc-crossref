{-# LANGUAGE RankNTypes #-}
module Text.Pandoc.CrossRef.Util.Util
  ( module Text.Pandoc.CrossRef.Util.Util
  , module Data.Generics
  ) where

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.Definition
import Data.Char (toUpper, toLower, isUpper)
import Data.List (intercalate, isSuffixOf, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Generics
import Text.Pandoc.Writers.LaTeX
import Data.Default

isFormat :: String -> Maybe Format -> Bool
isFormat fmt (Just (Format f)) = takeWhile (`notElem` "+-") f == fmt
isFormat _ Nothing = False

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs
capitalizeFirst [] = []

uncapitalizeFirst :: String -> String
uncapitalizeFirst (x:xs) = toLower x : xs
uncapitalizeFirst [] = []

isFirstUpper :: String -> Bool
isFirstUpper (x:_) = isUpper x
isFirstUpper [] = False

chapPrefix :: [Inline] -> Index -> [Inline]
chapPrefix delim index = intercalate delim (map (return . Str . uncurry (fromMaybe . show)) index)

data ReplacedResult a = Replaced Bool a | NotReplaced Bool
type GenRR m = forall a. Data a => (a -> m (ReplacedResult a))
newtype RR m a = RR {unRR :: a -> m (ReplacedResult a)}

runReplace :: (Monad m) => GenRR m -> GenericM m
runReplace f x = do
  res <- f x
  case res of
    Replaced True x' -> gmapM (runReplace f) x'
    Replaced False x' -> return x'
    NotReplaced True -> gmapM (runReplace f) x
    NotReplaced False -> return x

mkRR :: (Monad m, Typeable a, Typeable b)
     => (b -> m (ReplacedResult b))
     -> (a -> m (ReplacedResult a))
mkRR = extRR (const noReplaceRecurse)

extRR :: ( Monad m, Typeable a, Typeable b)
     => (a -> m (ReplacedResult a))
     -> (b -> m (ReplacedResult b))
     -> (a -> m (ReplacedResult a))
extRR def' ext = unRR (RR def' `ext0` RR ext)

replaceRecurse :: Monad m => a -> m (ReplacedResult a)
replaceRecurse = return . Replaced True

replaceNoRecurse :: Monad m => a -> m (ReplacedResult a)
replaceNoRecurse = return . Replaced False

noReplace :: Monad m => Bool -> m (ReplacedResult a)
noReplace recurse = return $ NotReplaced recurse

noReplaceRecurse :: Monad m => m (ReplacedResult a)
noReplaceRecurse = noReplace True

noReplaceNoRecurse :: Monad m => m (ReplacedResult a)
noReplaceNoRecurse = noReplace False

mkLaTeXLabel :: String -> String
mkLaTeXLabel l
 | null l = []
 | otherwise = "\\label{" ++ mkLaTeXLabel' l ++ "}"

mkLaTeXLabel' :: String -> String
mkLaTeXLabel' l =
  let ll = writeLaTeX def $ Pandoc nullMeta [Div (l, [], []) []]
  in takeWhile (/='}') . drop 1 . dropWhile (/='{') $ ll

getRefLabel :: String -> [Inline] -> Maybe String
getRefLabel _ [] = Nothing
getRefLabel tag ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , "}" `isSuffixOf` attr
  , ("{#"++tag++":") `isPrefixOf` attr
  = init `fmap` stripPrefix "{#" attr
getRefLabel _ _ = Nothing
