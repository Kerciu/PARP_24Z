module CommandHandler where
import Instructions (instructionsText)
import Inventory
import Locations
import Movement
import GameState
import Interactions
import Look
import Introduction
import Utils (printLines)

printInstructions :: IO ()
printInstructions = printLines instructionsText

handleInstructions :: GameState -> IO GameState
handleInstructions state = do
    printInstructions
    return state

handleStart :: GameState -> IO GameState
handleStart state = do
    let startState = initialState
    printIntroduction
    printInstructions
    lookAround startState
    return startState

handleQuit :: GameState -> IO GameState
handleQuit state = do
    putStrLn "Quitting the game. Goodbye!"
    return state { gameOver = True }

handleInventory :: GameState -> IO GameState
handleInventory state = do
    showInventory state
    return state

handleMovement :: String -> GameState -> IO GameState
handleMovement direction state = do
    move direction (locationName (currentLocation state)) state

handleItems :: String -> String -> GameState -> IO GameState
handleItems action object state = do
    case action of
        "take" -> takeItem object state
        "drop" -> dropItem object state
        "check" -> checkItem object state

handleLook :: GameState -> IO GameState
handleLook = lookAround

handleInteractions :: String -> GameState -> IO GameState
handleInteractions = interactWith

handleGive :: String -> String -> GameState -> IO GameState
handleGive = give

handleOpen :: String -> GameState -> IO GameState
handleOpen = open

handleEnterCar :: GameState -> IO GameState
handleEnterCar = enterCar

handleExitCar :: GameState -> IO GameState
handleExitCar = exitCar
