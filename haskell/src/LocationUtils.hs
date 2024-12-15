module LocationUtils where
import GameState
import Locations
import Objects
import Interactable

isAtLocation :: String -> GameState -> Bool
isAtLocation locationString state =
  case findLocation locationString of
    Nothing -> False
    Just loc -> locationName (currentLocation state) == locationName loc

hasItem :: String -> GameState -> Bool
hasItem item state =
    let itemObject = findItem item
    in case itemObject of
        Nothing -> False
        Just item -> item `elem` inventory state

isItemAt :: String -> Location -> Bool
isItemAt itemName location = any ((== itemName) . name) (locationInteractables location)