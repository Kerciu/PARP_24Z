import CommandHandler (printInstructions)
import CommandParser
import Control.Monad (when)
import GameState
import Introduction
import Look
import Utils (printLines)

readCommand :: IO String
readCommand = do
  putStr "> "
  getLine

-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: GameState -> IO ()
gameLoop state = do
  when (not (gameOver state)) $ do
    cmd <- readCommand
    newState <- parseCommand cmd state
    gameLoop newState
  when (gameOver state) $ putStrLn "Thanks for playing! Exiting the game..."

main = do
  printIntroduction
  printInstructions
  lookAround initialState
  gameLoop initialState
