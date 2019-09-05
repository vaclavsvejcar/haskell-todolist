module Main where

import           TodoList

main :: IO ()
main = do
    putStrLn "--- TODO APP v0.1.0 ---"
    let emptyList = []
    addItemI emptyList
    putStrLn "exitting..."
