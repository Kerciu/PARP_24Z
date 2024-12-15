module CommandParser (parseCommand) where

import GameState
import Objects
import Utils (printLines)
import Interactable
import CommandHandler
import qualified Data.Map as Map

parseCommand :: String -> GameState -> IO GameState
parseCommand cmd state = case words cmd of
    ["start"] -> handleStart state
    ["instructions"] -> handleInstructions state
    ["quit"] -> handleQuit state
    ["inventory"] -> handleInventory state
    ["n"] -> handleMovement "north" state
    ["s"] -> handleMovement "south" state
    ["e"] -> handleMovement "east" state
    ["w"] -> handleMovement "west" state
    ["take", object] -> handleItems "take" object state
    ["drop", object] -> handleItems "drop" object state
    ["check", object] -> handleItems "check" object state
    ["look"] -> handleLook state
    ["interact", character] -> handleInteractions character state
    ["give", character, object] -> handleGive character object state
    ["open", object] -> handleOpen object state
    ["enter_car"] -> handleEnterCar state
    ["exit_car"] -> handleExitCar state
    _ -> do
        printLines ["Unknown command.", ""]
        return state
