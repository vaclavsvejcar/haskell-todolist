module TodoList where

import           System.IO
import           Domain
import           InOut

-- Returns a new list of Items with the new item in it
addItem :: Item -> Items -> Items
addItem item items = item : items

-- Removes item specified by its index from items list
removeItem :: Int -> Items -> Either String Items
removeItem reverseIndex allItems = impl (length allItems - reverseIndex)
                                        allItems
 where
  impl index items = case (index, items) of
    (0, item : rest) -> Right rest
    (n, []         ) -> Left ("Index '" ++ show n ++ "'out of bounds.")
    (n, item : rest) -> case impl (n - 1) rest of
      Right newItems -> Right (item : newItems)
      Left  errMsg   -> Left errMsg

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
  "a" : "-" : item -> Right (AddItem (unwords item))
  ["d", idxStr]    -> if all (`elem` "0123456789") idxStr
    then Right (MarkAsDone (read idxStr))
    else Left "invalid index"
  ["l"] -> Right DisplayItems
  ["h"] -> Right Help
  ["q"] -> Right Quit
  other -> Left ("unknown command '" ++ unwords other ++ "'")

-- Displays prompt and reads user input
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Renders help with all available commands
renderHelp :: IO ()
renderHelp =
  putStrLn
    "Commands: a(dd item) - <ITEM NAME>, d(one) <INDEX>, l(list saved items), h(elp), q(uit)"

-- Handles interaction with user
interactWithUser :: IO ()
interactWithUser = do
  items <- readFromFile
  line  <- prompt "> "
  case parseCommand line of
    Right (AddItem item) -> do
      let newItems = addItem item items
      putStrLn ("Item '" ++ item ++ "' added")
      saveToFile newItems
      interactWithUser

    Right (MarkAsDone index) -> do
      let result = removeItem index items
      case result of
        Left error -> do
          putStrLn ("ERROR: " ++ error)
          interactWithUser
        Right newItems -> do
          putStrLn "Done."
          saveToFile newItems
          interactWithUser

    Right DisplayItems -> do
      putStrLn "Saved items to do:"
      putStrLn (renderItems items)
      interactWithUser

    Right Help -> do
      renderHelp
      interactWithUser

    Right Quit -> do
      putStrLn "Bye!"
      pure ()

    Left error -> do
      putStrLn ("ERROR: " ++ error)
      interactWithUser
