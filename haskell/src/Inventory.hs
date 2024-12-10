module Inventory where

import GameState
import Interactable

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