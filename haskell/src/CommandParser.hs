module CommandParser (parseCommand) where

import GameState
import Objects
import Utils (printLines)
import Interactable
import CommandHandler
import qualified Data.Map as Map

parseCommand :: String -> GameState -> IO GameState
parseCommand cmd state = case words cmd of
    ["instructions"] -> handleInstructions state
    ["quit"] -> handleQuit state
    ["inventory"] -> handleInventory state
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
