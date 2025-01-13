module Movement where

import Control.Monad.State (MonadState (put))
import Data.Map qualified as Map
import Endings
import GameState
import LocationUtils
import Locations
import Look
import Objects

move :: String -> String -> GameState -> IO GameState
move "east" "train_station" state = do
  putStrLn "Are you sure you want to escape the city? (yes/no)"
  confirmation <- getLine
  if confirmation == "yes"
    then do
      let message = escapeCityEndingText (hasItem "diary" state)
      putStrLn message
      let updatedState = state {gameOver = True}
      return updatedState
    else do
      lookAround state
move "north" "second_floor_of_hill_church" state =
  if "weird_box_opened" `elem` flags state
    then do
      let message = hillChurchEndingEscapeText
      putStrLn message
      let updatedState =
            state
              { gameOver = True
              }
      return updatedState
    else do
      putStrLn "You can't go that way"
      return state
move "north" "forest_cave" state = do
  if hasItem "amulet" state
    then do
      putStrLn "The amulet in your possession glows faintly, and you feel an ancient force giving way."
      putStrLn "The entrance opens, allowing you to pass into the unknown."
      if hasItem "engraved_ring" state
        then do
          let message = forestCaveEndingWeakenedText
          putStrLn message
        else do
          let message = forestCaveEndingKilledText
          putStrLn message
      let updatedState =
            state
              { gameOver = True
              }
      return updatedState
    else do
      putStrLn "A strange force seems to block your path. The entrance won’t budge."
      putStrLn "It feels like something is missing, something that could unlock the cave\'s secrets."
      return state
move strDirection currentLocationStr state =
  case parseDirection strDirection of
    Nothing ->
      putStrLn "Invalid direction" >> return state
    Just direction ->
      case Map.lookup direction (directions (currentLocation state)) of
        Just (nextLocationName, True) -> goNextLocation nextLocationName state
        Just (nextLocationName, False) -> locationBlocked nextLocationName state
        Nothing ->
          putStrLn "You can't go that way" >> return state

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
