module Inventory where

import GameState
import Interactable
import CommandParser

-- Handle taking an item
takeItem :: String -> GameState -> IO GameState
takeItem itemName state =
    case findItem itemName of
        Nothing -> noItemFound itemName
        Just item ->
            if item `elem` locationItems (currentLocation state)
            then do
                putStrLn $ "You have taken the " ++ name item ++ "."
                let updatedLocation = (currentLocation state) {
                        locationItems = filter (/= item) (locationItems $ currentLocation state)
                    }
                let updatedState = state {
                        currentLocation = updatedLocation,
                        inventory = item : inventory state
                    }
                return updatedState
            else noItemFound itemName
    where
        noItemFound itemName = do
            putStrLn $ "There is no '" ++ itemName ++ "' here."
            return state

-- Handle dropping an item
dropItem :: String -> GameState -> IO GameState
dropItem itemName state =
    case findItem itemName of
        Nothing -> noItemFound itemName
        Just item ->
            if item `elem` inventory state
            then do
                putStrLn $ "You have dropped the " ++ name item ++ "."
                let updatedLocation = (currentLocation state) {
                        locationItems = item : locationItems (currentLocation state)
                    }
                let updatedState = state {
                        currentLocation = updatedLocation,
                        inventory = filter (/= item) (inventory state)
                    }
                return updatedState
            else noItemFound itemName
    where
        noItemFound itemName = do
            putStrLn $ "You don't have the '" ++ itemName ++ "' in your inventory."
            return state

-- Handle checking inventory
showInventory :: GameState -> IO ()
showInventory state =
    case inventory state of
        [] -> putStrLn "Your inventory is empty."
        items -> do
            putStrLn "You are carrying the following items:"
            putStrLn (unlines (map name items))
