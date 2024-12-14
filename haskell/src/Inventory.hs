module Inventory where

import Data.Map qualified as Map
import GameState
import Interactable
import Locations
import Objects

addItemToLocation :: Interactable -> String -> GameState -> GameState
addItemToLocation item locationName state =
  let updatedItems = case Map.lookup locationName (locationItems state) of
        Nothing -> [item]
        Just items -> item : items
      updatedLocationItems = Map.insert locationName updatedItems (locationItems state)
   in state {locationItems = updatedLocationItems}

removeItemFromLocation :: Interactable -> String -> GameState -> GameState
removeItemFromLocation item locationName state =
  let updatedItems = case Map.lookup locationName (locationItems state) of
        Nothing -> []
        Just items -> filter (/= item) items
      updatedLocationItems = Map.insert locationName updatedItems (locationItems state)
   in state {locationItems = updatedLocationItems}

-- Handle taking an item
takeItem :: String -> GameState -> IO GameState
takeItem itemName state =
  case findItem itemName of
    Nothing -> do
      putStrLn $ "Item '" ++ itemName ++ "' does not exist."
      return state
    Just item ->
      let locationString = locationName (currentLocation state)
       in case Map.lookup locationString (locationItems state) of
            Nothing -> do
              putStrLn $ "There is no " ++ itemName ++ " here."
              return state
            Just items ->
              if item `elem` items
                then do
                  let updatedState = (removeItemFromLocation item locationString state) {inventory = item : inventory state}
                  putStrLn $ "You have taken the " ++ name item ++ "."
                  return updatedState
                else do
                  putStrLn $ "There is no " ++ itemName ++ " here."
                  return state

-- Handle dropping an item
dropItem :: String -> GameState -> IO GameState
dropItem itemName state =
  case findItem itemName of
    Nothing -> do
      putStrLn $ "Item '" ++ itemName ++ "' does not exist."
      return state
    Just item ->
      if item `elem` inventory state
        then do
          let locationString = locationName (currentLocation state)
          let updatedState = (addItemToLocation item locationString state) {inventory = filter (/= item) (inventory state)}
          putStrLn $ "You have dropped the " ++ name item ++ "."
          return updatedState
        else do
          putStrLn $ "You don't have " ++ itemName ++ " in your inventory."
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
