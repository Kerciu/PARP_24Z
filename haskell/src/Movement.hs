module Movement where

import GameState
import Locations
import qualified Data.Map as Map

move :: String -> Locations -> GameState -> IO GameState
move strDirection locations gameState =
    case parseDirection strDirection of
        Nothing ->
            putStrLn "Invalid direction" >> return gameState
        Just direction ->
            case nextRoom of
                Just (nextRoomName, True) -> goNextRoom nextRoomName
                Just (nextRoomName, False) -> roomBlocked nextRoomName
                Nothing ->
                    putStrLn "You can't go that way" >> return gameState
    
    where
        nextRoom = Map.lookup direction (directions (currentLocation gameState))
        
        goNextRoom nextRoomName =
            case Map.lookup nextRoomName locations of
                Just nextRoom ->
                    do
                        putStrLn $ locationDescription nextRoom
                        return gameState { currentLocation = nextRoom }
                Nothing ->
                    do
                        putStrLn "Error: Room not found!"
                        return gameState
        
        roomBlocked nextRoomName =
            putStrLn ("The way to " ++ nextRoomName ++ " is blocked.") >> return gameState
