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

{-# LANGUAGE RankNTypes #-}
module Text.Pandoc.CrossRef.Util.Util
  ( module Text.Pandoc.CrossRef.Util.Util
  , module Data.Generics
  ) where

import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.Definition
import Text.Pandoc.Class
import Text.Pandoc.Shared (Element(..))
import Data.Char (toUpper, toLower, isUpper)
import Data.Generics hiding (Prefix)
import Text.Pandoc.Writers.LaTeX
import Data.Default
import Data.Monoid ((<>))
import qualified Data.Text as T

intercalate' :: (Eq a, Monoid a, Foldable f) => a -> f a -> a
intercalate' s = foldr (\x acc -> if acc == mempty then x else x <> s <> acc) mempty

isFormat :: String -> Maybe Format -> Bool
isFormat fmt (Just (Format f)) = takeWhile (`notElem` "+-") f == fmt
isFormat _ Nothing = False

isLatexFormat :: Maybe Format -> Bool
isLatexFormat = isFormat "latex" `or'` isFormat "beamer"
  where a `or'` b = (||) <$> a <*> b

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs
capitalizeFirst [] = []

uncapitalizeFirst :: String -> String
uncapitalizeFirst (x:xs) = toLower x : xs
uncapitalizeFirst [] = []

isFirstUpper :: String -> Bool
isFirstUpper (x:_) = isUpper x
isFirstUpper [] = False

data ReplacedResult a = ReplacedRecurse Scope a
                      | NotReplacedRecurse Scope
                      | ReplacedNoRecurse a
                      | NotReplacedNoRecurse
type GenRR m = forall a. Data a => (Scope -> a -> m (ReplacedResult a))
newtype RR m a = RR {unRR :: Scope -> a -> m (ReplacedResult a)}

runReplace :: (Monad m) => Scope -> GenRR m -> GenericM m
runReplace s f x = do
  res <- f s x
  case res of
    ReplacedRecurse s' x' -> gmapM (runReplace s' f) x'
    ReplacedNoRecurse x' -> return x'
    NotReplacedRecurse s' -> gmapM (runReplace s' f) x
    NotReplacedNoRecurse -> return x

mkRR :: (Monad m, Typeable a, Typeable b)
     => (Scope -> b -> m (ReplacedResult b))
     -> (Scope -> a -> m (ReplacedResult a))
mkRR = extRR (\s _ -> noReplaceRecurse s)

extRR :: ( Monad m, Typeable a, Typeable b)
     => (Scope -> a -> m (ReplacedResult a))
     -> (Scope -> b -> m (ReplacedResult b))
     -> (Scope -> a -> m (ReplacedResult a))
extRR def' ext = unRR (RR def' `ext0` RR ext)

replaceRecurse :: Monad m => Scope -> a -> m (ReplacedResult a)
replaceRecurse s = return . ReplacedRecurse s

replaceNoRecurse :: Monad m => a -> m (ReplacedResult a)
replaceNoRecurse = return . ReplacedNoRecurse

noReplaceRecurse :: Monad m => Scope -> m (ReplacedResult a)
noReplaceRecurse = return . NotReplacedRecurse

noReplaceNoRecurse :: Monad m => m (ReplacedResult a)
noReplaceNoRecurse = return NotReplacedNoRecurse

mkLaTeXLabel :: String -> String
mkLaTeXLabel l
 | null l = []
 | otherwise = "\\label{" ++ mkLaTeXLabel' l ++ "}"

mkLaTeXLabel' :: String -> String
mkLaTeXLabel' l =
  let ll = either (error . show) T.unpack $
            runPure (writeLaTeX def $ Pandoc nullMeta [Div (l, [], []) []])
  in takeWhile (/='}') . drop 1 . dropWhile (/='{') $ ll

isSpace :: Inline -> Bool
isSpace = (||) <$> (==Space) <*> (==SoftBreak)

isLaTeXRawBlockFmt :: Format -> Bool
isLaTeXRawBlockFmt (Format "latex") = True
isLaTeXRawBlockFmt (Format "tex") = True
isLaTeXRawBlockFmt _ = False

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead x = Just $ head x

unhierarchicalize :: [Element] -> [Block]
unhierarchicalize (Sec l _n attr title body:xs) = Header l attr title : unhierarchicalize body ++ unhierarchicalize xs
unhierarchicalize (Blk bs:xs) = bs : unhierarchicalize xs
unhierarchicalize [] = []

newScope :: RefRec -> Scope -> Scope
newScope = (:)
