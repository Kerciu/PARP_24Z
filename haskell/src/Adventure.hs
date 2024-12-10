import GameState
-- The germ of a text adventure game
-- Marcin Szlenk 2024

introductionText :: [String]
introductionText = [
    "Welcome to the City of Shadows!",
    "",
    "Your adventure begins in a desolate town filled with strange locations, cryptic symbols, and unsettling figures.",
    "You find yourself at a train station, the clock forever stuck at 3:15 AM.",
    "An eerie silence lingers as the caretaker of the station watches you suspiciously.",
    "Resolving mystery of an ominous cult seem to be central to your quest.",
    "But be warned — the path ahead is fraught with danger and mystery.",
    "As you explore, remember that the choices you make could shape your fate.",
    "Good luck, adventurer — the mystery awaits you!",
    ""
    ]

instructionsText :: [String]
instructionsText = [
    "Available commands are:",
    "",
    "start                    -- to start the game.')",
    "n  s  e  w               -- to go in that direction.')",
    "take Object              -- to pick up an object.')",
    "drop Object              -- to put down an object.')",
    "check Object             -- to check object in inventory.')",
    "look                     -- to look around you again.')",
    "interact Character       -- to interact with characters.')",
    "give Character Object    -- to give object to character.')",
    "inventory                -- to check inventory contents.')",
    "instructions             -- to see this message again.')",
    "quit                     -- to end the game and quit.",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printIntroduction :: IO ()
printIntroduction = printLines introductionText
printInstructions :: IO ()
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs

initialState :: GameState
initialState = GameState {
    currentLocation = trainStation,
    inventory = [],
    drunkardInteraction = False,
    firstHomelessInteraction = False,
    secondHomelessInteraction = False,
    escapeCityEnding = False,
    hillChurchEndingEscape = False,
    forestCaveEndingKilled = False,
    forestCaveEndingWeakened = False,
    gameOver = False
}

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: GameState -> IO ()
gameLoop state = do
    cmd <- readCommand
    newState <- handleCommand cmd state
    gameLoop newState

main = do
    printIntroduction
    printInstructions
    gameLoop initialState

