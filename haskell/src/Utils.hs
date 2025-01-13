module Utils where

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
