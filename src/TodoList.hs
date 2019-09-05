module TodoList where

import           System.IO

type Item = String
type Items = [Item]

data Command
  = Quit
  | DisplayItems
  | AddItem String

-- Returns a new list of Items with the new item in it
addItem :: Item -> Items -> Items
addItem item items = item : items

-- Renders items into printable string
renderItems :: Items -> String
renderItems items =
  let renderItem index item = show index ++ " - " ++ item
      reversedItems = reverse items
      renderedItems = zipWith renderItem [1 ..] reversedItems
  in  unlines renderedItems

-- Parses command from user input
parseCommand :: String -> Either String Command
parseCommand line = case words line of
  [         "q"]    -> Right Quit
  [         "l"]    -> Right DisplayItems
  "a" : "-" :  item -> Right (AddItem (unwords item))
  other             -> Left ("unknown command '" ++ unwords other ++ "'")

-- Displays prompt and reads user input
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Handles interaction with user
interactWithUser :: Items -> IO ()
interactWithUser items = do
  putStrLn "\nCommands: q(uit), l(list saved items), a(dd item) - <ITEM NAME>"
  line <- prompt "> "
  case parseCommand line of
    Right Quit -> do
      putStrLn "Bye!"
      pure ()

    Right DisplayItems -> do
      putStrLn "Saved items to do:"
      putStrLn (renderItems items)
      interactWithUser items

    Right (AddItem item) -> do
      let newItems = addItem item items
      putStrLn ("Item '" ++ item ++ "' added")
      interactWithUser newItems

    Left error -> do
      putStrLn ("ERROR: " ++ error)
      interactWithUser items
