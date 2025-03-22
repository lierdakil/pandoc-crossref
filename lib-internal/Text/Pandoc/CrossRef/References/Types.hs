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

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes, DataKinds #-}
module Text.Pandoc.CrossRef.References.Types where

import Data.Default
import Numeric.Natural
import qualified Data.Map as M
import Data.Text (Text)
import Lens.Micro.GHC
import Lens.Micro.TH
import Text.Pandoc.Definition
import qualified Data.Sequence as S

type Index = S.Seq (Int, Maybe Text)

data RefRec = RefRec { refIndex :: Index
                     , refGlobal :: Natural
                     , refTitle :: [Inline]
                     , refSubfigure :: Maybe Index
                     } deriving (Show, Eq)

type RefMap = M.Map Text RefRec

data Prefix
  = PfxImg
  | PfxEqn
  | PfxTbl
  | PfxLst
  | PfxSec
  deriving (Eq, Ord, Enum, Bounded, Show)

-- state data type
data References = References { _stRefs :: M.Map Prefix RefMap
                             , _stCtrs :: M.Map Prefix Index
                             , _stGlob :: Natural
                             } deriving (Show, Eq)

instance Default References where
  def = References mempty mempty 0

makeLenses ''References

refsAt :: Prefix -> Lens' References RefMap
refsAt pfx = stRefs . at pfx . non mempty

ctrsAt :: Prefix -> Lens' References Index
ctrsAt pfx = stCtrs . at pfx . non mempty
