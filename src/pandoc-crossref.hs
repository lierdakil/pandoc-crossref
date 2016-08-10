import Text.Pandoc
import Text.Pandoc.JSON

import Text.Pandoc.CrossRef

main :: IO ()
main = toJSONFilter go
  where
    go fmt p@(Pandoc meta _) = runCrossRefIO meta fmt defaultCrossRefAction p
