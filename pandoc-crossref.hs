import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Control.Monad.State

import References
import Util.Settings
import Util.Options
import Util.CodeBlockCaptions
import Util.ModifyMeta

main :: IO ()
main = toJSONFilter go

go :: Maybe Format -> Pandoc -> IO Pandoc
go fmt (Pandoc meta bs) = do
  dtv <- getSettings meta
  let
    doWalk =
      bottomUpM (codeBlockCaptions opts) (walk divBlocks bs)
      >>= walkM (replaceBlocks opts)
      >>= bottomUpM (replaceRefs opts)
      >>= bottomUpM (listOf opts)
    opts = getOptions dtv fmt
    meta' = modifyMeta opts dtv
  return $ Pandoc meta' $ evalState doWalk def
