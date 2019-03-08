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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, TypeFamilies #-}

module Text.Pandoc.CrossRef.Util.Template
  ( Template
  , RefTemplate
  , BlockTemplate
  , MakeTemplate(..)
  , applyTemplate
  , applyRefTemplate
  , applyBlockTemplate
  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Generic
import Text.Pandoc.CrossRef.Util.Meta
import Text.Pandoc.CrossRef.Util.Settings.Types
import Text.Pandoc.CrossRef.Util.Template.Types
import Control.Applicative hiding (many, optional)
import Text.Read hiding ((<++), (+++))
import Data.Char (isAlphaNum, isUpper, toLower)
import Control.Monad ((<=<))
import Data.Data (Data)
import Text.ParserCombinators.ReadP

data PRVar = PRVar { prvName :: String
                   , prvIdx :: [[PRVar]]
                   } deriving Show
data ParseRes = ParseRes { prVar :: [PRVar]
                         , prPfx :: String
                         , prSfx :: String
                         } deriving Show

isVariableSym :: Char -> Bool
isVariableSym '.' = True
isVariableSym '_' = True
isVariableSym c = isAlphaNum c

parse :: ReadP ParseRes
parse = uncurry <$> (ParseRes <$> var) <*> option ("", "") ps <* eof
  where
    var = sepBy1 (PRVar <$> varName <*> many varIdx) (char '?')
    varName = munch1 isVariableSym
    varIdx = between (char '[') (char ']') var
    prefix = char '#' *> many (satisfy (/='%'))
    suffix = char '%' *> many (satisfy (/='#'))
    ps = (flip (,) <$> suffix <*> option "" prefix)
          +++ ((,) <$> prefix <*> option "" suffix)

instance MakeTemplate Template where
  type ElemT Template = Inlines
  makeTemplate dtv xs' = Template (genTemplate dtv xs')

instance MakeTemplate BlockTemplate where
  type ElemT BlockTemplate = Blocks
  makeTemplate dtv xs' = BlockTemplate (genTemplate dtv xs')

instance MakeTemplate RefTemplate where
  type ElemT RefTemplate = Inlines
  makeTemplate dtv xs' = RefTemplate $ \vars cap -> g (vf vars cap)
    where Template g = makeTemplate dtv xs'
          vf vars cap (vc:vs)
            | isUpper vc && cap = capitalize lookup' var
            | otherwise = lookup' var
            where
              var = toLower vc : vs
              lookup' x = vars x <|> lookupSettings x dtv
          vf _ _ [] = error "Empty variable name"

genTemplate :: (Data a) => Settings -> Many a -> VarFunc -> Many a
genTemplate dtv xs' vf = fromList $ scan (\var -> vf var <|> lookupSettings var dtv) $ toList xs'

scan :: (Data a) => VarFunc -> [a] -> [a]
scan = bottomUp . go
  where
  go vf (Math DisplayMath var:xs)
    | ParseRes{..} <- fst . head $ readP_to_S parse var
    = let replaceVar = maybe mempty (modifier . toInlines ("variable " ++ var))
          modifier = (<> text prSfx) . (text prPfx <>)
          tryVar PRVar{..} =
            case prvIdx of
              [] -> vf prvName
              idxVars ->
                let
                  idxs :: Maybe [String]
                  idxs = mapM (Just . toString ("index variables " ++ show idxVars) <=< tryVars) idxVars
                  arr = foldr (\i a -> getObjOrList i =<< a) (vf prvName) . reverse =<< idxs
                  getObjOrList :: String -> MetaValue -> Maybe MetaValue
                  getObjOrList i x = getObj i x <|> (readMaybe i >>= flip getList x)
                in arr
          tryVars = foldr ((<|>) . tryVar) Nothing

      in toList $ replaceVar (tryVars prVar) <> fromList xs
  go _ (x:xs) = toList $ singleton x <> fromList xs
  go _ [] = []
