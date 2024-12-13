module CommandHandler where
import Instructions (instructionsText)
import Inventory
import Locations
import Movement
import GameState
import Interactions
import Utils (printLines)

printInstructions :: IO ()
printInstructions = printLines instructionsText

handleInstructions :: GameState -> IO GameState
handleInstructions state = do
    printInstructions
    return state

handleQuit :: GameState -> IO GameState
handleQuit state = do
    printLines ["Quitting the game. Goodbye!"]
    return state

handleInventory :: GameState -> IO GameState
handleInventory state = do
    showInventory state
    return state

handleMovement :: String -> GameState -> IO GameState
handleMovement direction state = do
    move direction state
    handleLook state
    return state

handleItems :: String -> String -> GameState -> IO GameState
handleItems action object state = do
    printLines ["You " ++ action ++ " the " ++ object ++ "."]
    -- take, drop or check logic
    return state

handleLook :: GameState -> IO GameState
handleLook state = do
    printLines [locationDescription (location state)]
    return state

handleInteractions :: String -> GameState -> IO GameState
handleInteractions character state = do
    printLines ["Interacting with " ++ character ++ "..."]
    -- interactWith state character
    return state

handleGive :: String -> String -> GameState -> IO GameState
handleGive character object state = do
    printLines ["Giving " ++ object ++ " to " ++ character ++ "..."]
    newState <- give state character object
    return newState
