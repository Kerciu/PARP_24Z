module Movement where

import Data.Map qualified as Map
import GameState
import Locations
import Look
import Objects
import Control.Monad.State (MonadState(put))

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
        let updatedState = state {currentLocation = nextLocation}
        lookAround updatedState
    Nothing ->
      do
        putStrLn "Error: Location not found!"
        return state

locationBlocked :: String -> GameState -> IO GameState
locationBlocked nextLocationName state =
  if nextLocationName `elem` closedLocations state
    then case nextLocationName of
      "hotel_room" ->
        openHotelRoom state
      "hotel_basement" ->
        openHotelBasement state
      "archive" ->
        openArchive state
      _ ->
        putStrLn "Error: Don't know how to open it" >> return state
    else
      goNextLocation nextLocationName state

openHotelRoom :: GameState -> IO GameState
openHotelRoom state = putStrLn "Unlocking the hotel room is not implemented yet." >> return state

openHotelBasement :: GameState -> IO GameState
openHotelBasement state = putStrLn "Unlocking the hotel basement is not implemented yet." >> return state

openArchive :: GameState -> IO GameState
openArchive state = putStrLn "Unlocking the archive is not implemented yet." >> return state
