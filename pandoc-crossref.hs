import Text.Pandoc
import Text.Pandoc.JSON

import Text.Pandoc.CrossRef

main :: IO ()
main = toJSONFilter go
  where
    go fmt p@(Pandoc meta _) = runCrossRefIO meta fmt action p
      where
        action (Pandoc _ bs) = do
          meta' <- crossRefMeta
          bs' <- crossRefBlocks bs
          return $ Pandoc meta' bs'
