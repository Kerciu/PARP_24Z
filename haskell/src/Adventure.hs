-- The germ of a text adventure game
-- Marcin Szlenk 2024

introductionText :: [String]
introductionText = [
    "A long time ago, in a galaxy far, far away...",
    "",
    "It is a period of civil war. Rebel",
    "spaceships, striking from a hidden",
    "base, have won their first victory",
    "against the evil Galactic Empire.",
    "",
    "During the battle, Rebel spies managed",
    "to steal secret plans to the Empire's",
    "ultimate weapon, the Death Star, an",
    "armored space station with enough",
    "power to destroy an entire planet.",
    "",
    "Pursued by the Empire's sinister agents,",
    "Princess Leia races home aboard her",
    "starship, custodian of the stolen plans",
    "that can save her people and restore",
    "freedom to the galaxy....",
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
    
-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
    cmd <- readCommand
    case cmd of
        "instructions" -> do printInstructions
                             gameLoop
        "quit" -> return ()
        _ -> do printLines ["Unknown command.", ""]
                gameLoop

main = do
    printIntroduction
    printInstructions
    gameLoop

