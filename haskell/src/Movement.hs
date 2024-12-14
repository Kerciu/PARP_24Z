module Movement where

import Control.Monad.State (MonadState (put))
import Data.Map qualified as Map
import GameState
import Locations
import Look
import Objects

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
openHotelRoom state = do
  putStrLn "The door is locked."
  putStrLn "However, you noticed a sign that says 'Michael Turner'."
  putStrLn "There is also a keypad next to the door."
  putStrLn "Please enter 4-digit code:"
  code <- getLine
  if code == "1974"
    then do
      let updatedState =
            state
              { closedLocations = filter (/= "hotel_room") (closedLocations state)
              }
      putStrLn "Correct code!"
      goNextLocation "hotel_room" updatedState
    else do
      putStrLn "Wrong code!"
      return state

openHotelBasement :: GameState -> IO GameState
openHotelBasement state =
  if redFuse `elem` inventory state
    && blueFuse `elem` inventory state
    && greenFuse `elem` inventory state
    then do
      let updatedState =
            state
              { inventory = filter (`notElem` [redFuse, greenFuse, blueFuse]) (inventory state),
                closedLocations = filter (/= "hotel_basement") (closedLocations state)
              }
      putStrLn "You placed the fuses in the fuse box and the elevator started working."
      goNextLocation "hotel_basement" updatedState
    else do
      putStrLn "The elevator is not working."
      putStrLn "However, you noticed a fuse box next to the elevator with 3 fuses missing."
      putStrLn "Maybe you can find them somewhere."
      return state

openArchive :: GameState -> IO GameState
openArchive state =
  if key `elem` inventory state
    then do
      let updatedState =
            state
              { inventory = filter (/= key) (inventory state),
                closedLocations = filter (/= "archive") (closedLocations state)
              }
      putStrLn "You have unlocked the archive."
      goNextLocation "archive" updatedState
    else do
      putStrLn "The archive is locked. You need a key to open it."
      return state
