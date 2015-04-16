module References.Types ( References(..)
                        , WS
                        , RefRec(..)
                        , RefMap
                        ) where

import qualified Data.Map as M
import Text.Pandoc.Definition
import Control.Monad.State

data RefRec = RefRec { refIndex :: (Int, Int)
                     , refTitle :: [Inline]
                     }

type RefMap = M.Map String RefRec

-- state data type
data References = References { imgRefs :: RefMap
                             , eqnRefs :: RefMap
                             , tblRefs :: RefMap
                             , curChap :: Int
                             }

--state monad
type WS a = State References a
