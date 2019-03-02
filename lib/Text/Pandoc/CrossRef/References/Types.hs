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

{-# LANGUAGE TemplateHaskell #-}
module Text.Pandoc.CrossRef.References.Types where

import qualified Data.Map as M
import Control.Monad.State
import Data.Default
import Data.Accessor.Template
import Text.Pandoc.Builder

data RefRec = RefRec { refIndex :: !Int -- global ordinal number for prefix
                     , refIxInl :: Inlines -- index as inilnes
                     , refTitle :: !Inlines -- title text
                     , refScope :: !(Maybe RefRec) -- reference to parent label
                     , refLevel :: !Int -- number of upper scopes of the same prefix
                     , refLabel :: !String -- label, i.e. pfx:label string
                     , refPfx   :: !String -- reference prefix, the part in label before :
                     , refCaption :: Inlines -- caption after applying template; must be non-strict
                     } deriving (Show, Eq, Ord)

type RefMap = M.Map String RefRec

-- state data type
data References = References { referenceData_ :: !RefMap
                             , pfxCounter_ :: !(M.Map String CounterRec)
                             } deriving (Show, Eq)

data CounterRec = CounterRec {
    crIndex :: Int
  , crIndexInScope :: M.Map (Maybe RefRec) Int
  } deriving (Show, Eq)

--state monad
type WS a = State References a

type Scope = [RefRec]

instance Default References where
  def = References M.empty M.empty

instance Default CounterRec where
  def = CounterRec 0 M.empty

deriveAccessors ''References
