module InOut where

import           Domain
import           System.Directory               ( doesFileExist )
import qualified System.IO.Strict              as S

saveFileName = "todo.txt"

-- Saves TODO items to file
saveToFile :: Items -> IO ()
saveToFile items = writeFile saveFileName (unlines items)

-- Reads TODO items from file
readFromFile :: IO Items
readFromFile = do
  fileExists <- doesFileExist saveFileName
  if fileExists
    then do
      content <- S.readFile saveFileName
      return (lines content)
    else pure []
