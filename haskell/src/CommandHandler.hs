module CommandHandler where
import Instructions (instructionsText)
import GameState
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
    -- inventory printing logic
    return state

handleMovement :: String -> GameState -> IO GameState
handleMovement direction state = do
    printLines ["Moving " ++ direction ++ "..."]
    -- movement logic
    return state

handleItems :: String -> String -> GameState -> IO GameState
handleItems action object state = do
    printLines ["You " ++ action ++ " the " ++ object ++ "."]
    -- take, drop or check logic
    return state

handleLook :: GameState -> IO GameState
handleLook state = do
    printLines ["You look around..."]
    -- looking around logic
    return state

handleInteractions :: String -> GameState -> IO GameState
handleInteractions character state = do
    printLines ["Interacting with " ++ character ++ "..."]
    -- character interactions logic
    return state

handleGive :: String -> String -> GameState -> IO GameState
handleGive character object state = do
    printLines ["Giving " ++ object ++ " to " ++ character ++ "..."]
    -- item giving logic
    return state
