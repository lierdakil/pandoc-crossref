module References.Types ( References(..)
                        , WS
                        , RefRec(..)
                        , RefMap
                        , def
                        ) where

import qualified Data.Map as M
import Text.Pandoc.Definition
import Control.Monad.State
import Data.Default

data RefRec = RefRec { refIndex :: ([Int], Int)
                     , refTitle :: [Inline]
                     } deriving (Show, Eq)

type RefMap = M.Map String RefRec

-- state data type
data References = References { imgRefs :: RefMap
                             , eqnRefs :: RefMap
                             , tblRefs :: RefMap
                             , lstRefs :: RefMap
                             , curChap :: [Int]
                             } deriving (Show, Eq)

--state monad
type WS a = State References a

instance Default References where
  def = References n n n n []
    where n = M.empty
