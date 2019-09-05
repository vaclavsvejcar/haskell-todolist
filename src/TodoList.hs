module TodoList where

type Item = String
type Items = [Item]

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

-- Interacts with user, adds new item into given items list
addItemI :: Items -> IO Items
addItemI items = do
  putStrLn "Enter new item to add to your TODO list:"
  item <- getLine
  let newItems = addItem item items
  putStrLn ("Item '" ++ item ++ "' added\n")
  putStrLn "Your current TODO list is:"
  putStrLn (renderItems newItems)
  addItemI newItems
