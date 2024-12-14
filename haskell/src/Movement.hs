module Movement where

import GameState
import Locations
import qualified Data.Map as Map

move :: String -> GameState -> IO GameState
move strDirection state =
    case parseDirection strDirection of
        Nothing ->
            putStrLn "Invalid direction" >> return state
        Just direction ->
            case Map.lookup direction (directions (currentLocation state)) of
                Just (nextLocationName, True) -> goNextLocation nextLocationName state
                Just (nextLocationName, False) -> locationBlocked nextLocationName state
                Nothing ->
                    putStrLn "You can't go that way" >> return state
    

goNextLocation :: String -> GameState -> IO GameState
goNextLocation nextLocationName state =
    case findLocation nextLocationName of
        Just nextLocation ->
            do
                putStrLn $ locationDescription nextLocation
                return state { currentLocation = nextLocation }
        Nothing ->
            do
                putStrLn "Error: Location not found!"
                return state

locationBlocked :: String -> GameState -> IO GameState
locationBlocked nextLocationName state =
    putStrLn ("The way to " ++ nextLocationName ++ " is blocked.") >> return state
