module References.List (listOf) where

import Text.Pandoc.Definition
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import References.Types
import Util.Util
import Util.Options

listOf :: Options -> [Block] -> WS [Block]
listOf Options{outFormat=Just f} x | isFormat "latex" f = return x
listOf opts (Para [RawInline (Format "tex") "\\listoffigures"]:xs)
  = gets imgRefs >>= makeList (lofTitle opts) xs
listOf opts (Para [RawInline (Format "tex") "\\listoftables"]:xs)
  = gets tblRefs >>= makeList (lotTitle opts) xs
listOf _ x = return x

makeList :: [Block] -> [Block] -> M.Map String RefRec -> WS [Block]
makeList title xs refs
  = return $
      title ++
      OrderedList style (item `map` refsSorted)
      : xs
  where
    refsSorted = sortBy compare' $ M.toList refs
    compare' (_,RefRec{refIndex=i}) (_,RefRec{refIndex=j}) = compare i j
    item = (:[]) . Plain . refTitle . snd
    style = (1,DefaultStyle,DefaultDelim)
