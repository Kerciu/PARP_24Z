module CommandHandler where
import Instructions (instructionsText)
import Inventory
import Locations
import Movement
import GameState
import Interactions
import Look
import Utils (printLines)

printInstructions :: IO ()
printInstructions = printLines instructionsText

handleInstructions :: GameState -> IO GameState
handleInstructions state = do
    printInstructions
    return state

handleQuit :: GameState -> IO GameState
handleQuit state = do
    putStrLn "Quitting the game. Goodbye!"
    return state { gameOver = True }

handleInventory :: GameState -> IO GameState
handleInventory state = do
    showInventory state
    return state

handleMovement :: String -> GameState -> IO GameState
handleMovement = move

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
