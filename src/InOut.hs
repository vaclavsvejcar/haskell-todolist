module InOut where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Maybe
import           Domain
import           System.Directory               ( doesFileExist )
import qualified System.IO.Strict              as S

saveFileName = "todo-saved.json"

-- Saves TODO items into JSON file
saveToFile :: Items -> IO ()
saveToFile items = writeFile saveFileName (C.unpack (encode (reverse items)))

-- Reads TODO items from JSON file
readFromFile :: IO Items
readFromFile = do
  fileExists <- doesFileExist saveFileName
  if fileExists
    then do
      content <- S.readFile saveFileName
      let decoded = fromMaybe [] (decode (C.pack content) :: Maybe Items)
      return (reverse decoded)
    else pure []
