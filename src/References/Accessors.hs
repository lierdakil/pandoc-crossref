module References.Accessors where

import References.Types
import Util.Accessor
import Text.Pandoc.Definition (nullMeta)
import Data.Map (empty)

imgRefs' :: Accessor References RefMap
imgRefs' new r@References{imgRefs=old} = (old, r{imgRefs=new})

eqnRefs' :: Accessor References RefMap
eqnRefs' new r@References{eqnRefs=old} = (old, r{eqnRefs=new})

tblRefs' :: Accessor References RefMap
tblRefs' new r@References{tblRefs=old} = (old, r{tblRefs=new})

defaultReferences :: References
defaultReferences = References empty empty empty 0 (const Nothing) nullMeta
