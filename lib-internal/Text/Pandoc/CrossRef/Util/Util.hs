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

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Text.Pandoc.CrossRef.Util.Util where

import Text.Pandoc.CrossRef.References.Types.Ref
import Text.Pandoc.Definition
import Text.Pandoc.Class
import Data.Char (toUpper, toLower, isUpper)
import Text.Pandoc.Writers.LaTeX
import Data.Default
import qualified Data.Text as T

import qualified Data.Accessor.Basic as Accessor
import qualified Control.Monad.State as State

intercalate' :: (Eq a, Monoid a, Foldable f) => a -> f a -> a
intercalate' s xs
  | null xs = mempty
  | otherwise = foldr1 (\x acc -> x <> s <> acc) xs

isFormat :: T.Text -> Maybe Format -> Bool
isFormat fmt (Just (Format f)) = T.takeWhile (`notElem` ("+-" :: String)) f == fmt
isFormat _ Nothing = False

isLatexFormat :: Maybe Format -> Bool
isLatexFormat = isFormat "latex" `or'` isFormat "beamer"
  where a `or'` b = (||) <$> a <*> b

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

mkLaTeXLabel :: T.Text -> T.Text
mkLaTeXLabel l
 | T.null l = ""
 | otherwise = "\\label{" <> mkLaTeXLabel' l <> "}"

mkLaTeXLabel' :: T.Text -> T.Text
mkLaTeXLabel' l =
  let ll = either (error . show) id $
            runPure (writeLaTeX def $ Pandoc nullMeta [Div (l, [], []) []])
  in T.takeWhile (/='}') . T.drop 1 . T.dropWhile (/='{') $ ll

isSpace :: Inline -> Bool
isSpace = (||) <$> (==Space) <*> (==SoftBreak)

isLaTeXRawBlockFmt :: Format -> Bool
isLaTeXRawBlockFmt (Format "latex") = True
isLaTeXRawBlockFmt (Format "tex") = True
isLaTeXRawBlockFmt _ = False

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead x = Just $ head x

unhierarchicalize :: [Block] -> [Block]
unhierarchicalize
  (Div (dident, "section":dcls, dkvs) (Header level (hident,hcls,hkvs) ils : xs) : ys)
  | T.null hident, dkvs == hkvs, dcls == hcls = Header level (dident, hcls, hkvs) ils : unhierarchicalize xs <> unhierarchicalize ys
unhierarchicalize (x:xs) = x : unhierarchicalize xs
unhierarchicalize [] = []

newScope :: RefRec -> Scope -> Scope
newScope = (:)

-- * accessors in the form of actions in the state monad

set :: (State.MonadState r s) => Accessor.T r a -> a -> s ()
set f x = State.modify (Accessor.set f x)

get :: (State.MonadState r s) => Accessor.T r a -> s a
get f = State.gets (Accessor.get f)

modify :: (State.MonadState r s) => Accessor.T r a -> (a -> a) -> s ()
modify f g = State.modify (Accessor.modify f g)

simpleTable :: [Alignment] -> [Double] -> [[[Block]]] -> Block
simpleTable align width bod = Table nullAttr noCaption (zip align $ map ColWidth width)
  noTableHead [mkBody bod] noTableFoot
  where
  mkBody xs = TableBody nullAttr (RowHeadColumns 0) [] (map mkRow xs)
  mkRow xs = Row nullAttr (map mkCell xs)
  mkCell xs = Cell nullAttr AlignDefault (RowSpan 0) (ColSpan 0) xs
  noCaption = Caption Nothing mempty
  noTableHead = TableHead nullAttr []
  noTableFoot = TableFoot nullAttr []
