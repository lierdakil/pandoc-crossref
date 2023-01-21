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

import Data.Default
import qualified Data.Map as M
import Data.Text (Text)
import Lens.Micro.TH
import Text.Pandoc.Definition

type Index = [(Int, Maybe Text)]

data RefRec = RefRec { refIndex :: Index
                     , refTitle :: [Inline]
                     , refSubfigure :: Maybe Index
                     } deriving (Show, Eq)

type RefMap = M.Map Text RefRec

-- state data type
data References = References { _imgRefs :: RefMap
                             , _eqnRefs :: RefMap
                             , _tblRefs :: RefMap
                             , _lstRefs :: RefMap
                             , _secRefs :: RefMap
                             , _curChap :: Index
                             } deriving (Show, Eq)

instance Default References where
  def = References n n n n n []
    where n = M.empty

makeLenses ''References
