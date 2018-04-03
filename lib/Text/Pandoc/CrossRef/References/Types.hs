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

type Index = [(Int, String)]

data RefRec = RefRec { refIndex :: !Index
                     , refTitle :: !Inlines
                     , refSubfigure :: !(Maybe Index)
                     } deriving (Show, Eq)

type RefMap = M.Map String RefRec

-- state data type
data References = References { referenceData_ :: !RefMap
                             , curChap_ :: !(M.Map String Index)
                             , pfxCounter_ :: !(M.Map String Int)
                             } deriving (Show, Eq)

--state monad
type WS a = State References a

instance Default References where
  def = References M.empty M.empty M.empty

deriveAccessors ''References
