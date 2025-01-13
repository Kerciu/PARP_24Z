module LocationUtils where
import GameState
import Locations
import Objects
import Interactable
import qualified Data.Map as Map

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

isItemAt :: Interactable -> String -> GameState -> Bool
isItemAt item location state = do
  case Map.lookup location (locationItems state) of
    Nothing -> False
    Just items -> item `elem` items
