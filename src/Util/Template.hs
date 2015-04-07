module Util.Template where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import References.Types

import Control.Monad.State
import Data.Maybe
import Util.Util
import Util.Meta

replaceTemplate :: [Inline] -> WS [Inline]
replaceTemplate = bottomUpM replace
  where
  replace (x@(Math DisplayMath var):xs) = (++xs) `fmap` getTemplateMetaVar var [x]
  replace x = return x

getTemplateMetaVar :: String -> [Inline] -> WS [Inline]
getTemplateMetaVar var def' = do
  meta <- gets stMeta
  tmplv <- gets stTmplV
  dtv <- gets stDTV
  return
    $ fromMaybe def'
    $ (tmplv var `mplus` lookupDefault var meta dtv) >>= toInlines

applyTemplate :: [Inline] -> [Inline] -> [Inline] -> WS [Inline]
applyTemplate i t tmpl = withTmplV internalVars $ replaceTemplate tmpl
  where withTmplV f g = do
          modify $ \s -> s{stTmplV=f}
          res <- g
          modify $ \s -> s{stTmplV=const Nothing}
          return res
        internalVars "i" = Just $ MetaInlines i
        internalVars "t" = Just $ MetaInlines t
        internalVars _   = Nothing
