module Inventory where

import GameState
import Locations
import Objects
import Interactable

addItemToLocation :: Location -> Interactable -> Location
addItemToLocation loc item = loc { locationItems = item : locationItems loc }

removeItemFromLocation :: Location -> Interactable -> Location
removeItemFromLocation loc item = loc { locationItems = filter (/= item) (locationItems loc) }

-- Handle taking an item
takeItem :: String -> GameState -> IO GameState
takeItem itemName state =
    case findItem itemName of
        Nothing -> noItemFound itemName
        Just item ->
            if item `elem` locationItems (currentLocation state)
            then do
                let updatedLocation = removeItemFromLocation (currentLocation state) item
                let updatedState = state {
                        currentLocation = updatedLocation,
                        inventory = item : inventory state
                    }
                putStrLn $ "You have taken the " ++ name item ++ "."
                putStrLn $ description item
                return updatedState
            else noItemFound itemName
    where
        noItemFound itemName = do
            putStrLn $ "There is no " ++ itemName ++ " here."
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
                let updatedLocation = addItemToLocation (currentLocation state) item
                let updatedState = state {
                        currentLocation = updatedLocation,
                        inventory = filter (/= item) (inventory state)
                    }
                return updatedState
            else noItemFound itemName
    where
        noItemFound itemName = do
            putStrLn $ "You don't have the " ++ itemName ++ " in your inventory."
            return state

-- Handle checking an item
checkItem :: String -> GameState -> IO GameState
checkItem itemName state =
    case findItem itemName of
        Nothing -> noItemFound itemName
        Just item ->
            if item `elem` inventory state
            then do
                putStrLn $ description item
                return state
            else noItemFound itemName
    where
        noItemFound itemName = do
            putStrLn $ "You are not holding the " ++ itemName ++ "."
            return state

-- Handle checking inventory
showInventory :: GameState -> IO ()
showInventory state =
    case inventory state of
        [] -> putStrLn "Your inventory is empty."
        items -> do
            putStrLn "You are carrying the following items:"
            putStrLn (unlines (map name items))

-- Display description of an item if it is in inventory
displayItemDescription :: String -> GameState -> IO ()
displayItemDescription itemName state =
    case findItem itemName of
        Nothing -> noItemFound itemName
        Just item ->
            if item `elem` inventory state
            then putStrLn $ description item
            else noItemFound itemName
    where
        noItemFound itemName =
           putStrLn $ "You are not holding the " ++ itemName ++ "."
