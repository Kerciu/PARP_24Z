module Movement where

import GameState
import Locations
import qualified Data.Map as Map

move :: String -> GameState -> IO GameState
move strDirection gameState =
    case parseDirection strDirection of
        Nothing ->
            putStrLn "Invalid direction" >> return gameState
        Just direction ->
            case Map.lookup direction (directions (location gameState)) of
                Just (nextLocationName, True) -> goNextLocation nextLocationName
                Just (nextLocationName, False) -> locationBlocked nextLocationName
                Nothing ->
                    putStrLn "You can't go that way" >> return gameState
    
    where
        goNextLocation nextLocationName =
            case Map.lookup nextLocationName locations of
                Just nextLocation ->
                    do
                        putStrLn $ locationDescription nextLocation
                        return gameState { location = nextLocation }
                Nothing ->
                    do
                        putStrLn "Error: Location not found!"
                        return gameState
        
        locationBlocked nextLocationName =
            putStrLn ("The way to " ++ nextLocationName ++ " is blocked.") >> return gameState
