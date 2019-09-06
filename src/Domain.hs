module Domain where

type Item = String
type Items = [Item]

data Command
  = AddItem String
  | MarkAsDone Int
  | DisplayItems
  | Help
  | Quit
