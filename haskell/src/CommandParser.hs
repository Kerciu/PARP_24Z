module CommandParser (handleCommand) where

import GameState
import Objects
import Utils (printLines)

handleCommand :: String -> GameState -> IO GameState
handleCommand cmd state = case words cmd of
    ["instructions"] -> handleInstructions state
    ["quit"] -> handleQuit state
    ["inventory"] -> handleInventory state
    ["n"] -> handleMovement "North" state
    ["s"] -> handleMovement "South" state
    ["e"] -> handleMovement "East" state
    ["w"] -> handleMovement "West" state
    ["n"] -> handleMovement "North" state
    ["s"] -> handleMovement "South" state
    ["e"] -> handleMovement "East" state
    ["w"] -> handleMovement "West" state
    ["take", object] -> handleItems "take" object state
    ["drop", object] -> handleItems "drop" object state
    ["check", object] -> handleItems "check" object state
    ["look"] -> handleLook state
    ["interact", character] -> handleInteractions character state
    ["give", character, object] -> handleGive character object state
    _ -> do
        printLines ["Unknown command.", ""]
        return state

findItem :: String -> Maybe Interactable
findItem itemName = Map.lookup itemName itemsMap
  where
    itemsMap = Map.fromList
        [ ("Cigarettes", cigarettes)
        , ("WeirdBox", weirdBox)
        , ("Harnas", harnas)
        , ("KufloweMocne", kufloweMocne)
        , ("CarKeys", carKeys)
        , ("Amulet", amulet)
        , ("Diary", diary)
        , ("Notes", notes)
        , ("Newspaper", newspaper)
        , ("LeafWithCode", leafWithCode)
        , ("EngravedRing", engravedRing)
        , ("Key", key)
        ]
