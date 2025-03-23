{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module Text.Pandoc.CrossRef.Util.Util
  ( module Text.Pandoc.CrossRef.Util.Util
  , module Data.Generics
  ) where

import Data.Char (isUpper, toLower, toUpper)
import Data.Default
import Data.Generics
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Pandoc.Builder hiding ((<>))
import Text.Pandoc.Class
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.Writers.LaTeX
import qualified Data.Sequence as S

intercalate' :: (Eq a, Monoid a, Foldable f) => a -> f a -> a
intercalate' s xs
  | null xs = mempty
  | otherwise = foldr1 (\x acc -> x <> s <> acc) xs

isFormat :: T.Text -> Maybe Format -> Bool
isFormat fmt (Just (Format f)) = T.takeWhile (`notElem` ("+-" :: String)) f == fmt
isFormat _ Nothing = False

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t
  | Just (x, xs) <- T.uncons t = toUpper x `T.cons` xs
  | otherwise = T.empty

uncapitalizeFirst :: T.Text -> T.Text
uncapitalizeFirst t
  | Just (x, xs) <- T.uncons t = toLower x `T.cons` xs
  | otherwise = T.empty

isFirstUpper :: T.Text -> Bool
isFirstUpper xs
  | Just (x, _) <- T.uncons xs  = isUpper x
  | otherwise = False

chapPrefix :: [Inline] -> Index -> [Inline]
chapPrefix delim = toList
  . intercalate' (fromList delim)
  . fmap str
  . S.filter (not . T.null)
  . fmap (uncurry (fromMaybe . T.pack . show))

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

mkLaTeXLabel :: T.Text -> T.Text
mkLaTeXLabel l
 | T.null l = ""
 | otherwise = "\\label{" <> mkLaTeXLabel' l <> "}"

mkLaTeXLabel' :: T.Text -> T.Text
mkLaTeXLabel' l =
  let ll = either (error . show) id $
            runPure (writeLaTeX def $ Pandoc nullMeta [Div (l, [], []) []])
  in T.takeWhile (/='}') . T.drop 1 . T.dropWhile (/='{') $ ll

getRefLabel :: Prefix -> [Inline] -> Maybe T.Text
getRefLabel _ [] = Nothing
getRefLabel tag ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , "}" `T.isSuffixOf` attr
  , ("{#"<> pfxText tag <>":") `T.isPrefixOf` attr
  = T.init `fmap` T.stripPrefix "{#" attr
getRefLabel _ _ = Nothing

isSpace :: Inline -> Bool
isSpace = (||) <$> (==Space) <*> (==SoftBreak)

isLaTeXRawBlockFmt :: Format -> Bool
isLaTeXRawBlockFmt (Format "latex") = True
isLaTeXRawBlockFmt (Format "tex") = True
isLaTeXRawBlockFmt _ = False
