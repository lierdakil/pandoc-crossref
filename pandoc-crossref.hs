import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Control.Monad.State

import References
import Util.Default.Settings
import Util.Options

main :: IO ()
main = toJSONFilter go

go :: Maybe Format -> Pandoc -> IO Pandoc
go fmt (Pandoc meta bs) = do
  dtv <- getDefaultSettings meta
  let
    st = defaultReferences{stMeta=meta, stDTV=dtv}
    doWalk =
      walkM (replaceBlocks opts) bs
      >>= bottomUpM (replaceRefs opts)
      >>= bottomUpM (listOf opts)
    opts = getOptions meta dtv fmt
  return $ Pandoc meta $ evalState doWalk st
