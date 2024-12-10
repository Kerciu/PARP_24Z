module CommandParser (handleCommand) where

import GameState
import Objects

handleCommand :: String -> GameState -> IO GameState
handleCommand cmd state = case words cmd of
    ["instructions"] -> do
        printInstructions
        return state
    ["quit"] -> do
        putStrLn "Goodbye!"
        return state
    ["inventory"] -> do
        handleInventoryCommand state
        return state
    _ -> do
        printLines ["Unknown command.", ""]
        return state

findItem :: String -> Maybe Interactable
findItem itemName =
    case itemName of
        "Cigarettes" -> Just cigarettes
        "WeirdBox" -> Just weirdBox
        "Harnas" -> Just harnas
        "KufloweMocne" -> Just kufloweMocne
        "CarKeys" -> Just carKeys
        "Amulet" -> Just amulet
        "Diary" -> Just diary
        "Notes" -> Just notes
        "Newspaper" -> Just newspaper
        "LeafWithCode" -> Just leafWithCode
        "EngravedRing" -> Just engravedRing
        "Key" -> Just key
        _ -> Nothing
