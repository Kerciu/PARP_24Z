module Utils where

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

hasItem :: GameState -> String -> Bool
hasItem state item = item `elem` inventory state

hasFlag :: GameState -> String -> Bool
hasFlag state flag = flag `elem` flags state

addItem :: GameGame -> String -> IO ()
addItem game item = modifyState game $ \state -> state { inventory = item : inventory state }

removeItem :: GameGame -> String -> IO ()
removeItem game item = modifyState game $ \state -> state { inventory = filter (/= item) (inventory state) }

addFlag :: GameGame -> String -> IO ()
addFlag game flag = modifyState game $ \state -> state { flags = flag : flags state }