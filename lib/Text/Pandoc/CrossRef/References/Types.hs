{-# LANGUAGE TemplateHaskell #-}
module Text.Pandoc.CrossRef.References.Types where

import qualified Data.Map as M
import Text.Pandoc.Definition
import Control.Monad.State
import Data.Default
import Data.Accessor.Template

type Index = [(Int, Maybe String)]

data RefRec = RefRec { refIndex :: Index
                     , refTitle :: [Inline]
                     , refSubfigure :: Maybe Index
                     } deriving (Show, Eq)

type RefMap = M.Map String RefRec

-- state data type
data References = References { imgRefs_ :: RefMap
                             , eqnRefs_ :: RefMap
                             , tblRefs_ :: RefMap
                             , lstRefs_ :: RefMap
                             , secRefs_ :: RefMap
                             , curChap_ :: Index
                             , subFig_  :: Bool
                             } deriving (Show, Eq)

--state monad
type WS a = State References a

instance Default References where
  def = References n n n n n [] False
    where n = M.empty

deriveAccessors ''References
