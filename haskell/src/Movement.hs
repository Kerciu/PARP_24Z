module Movement where

import Control.Monad.State (MonadState (put))
import Data.Map qualified as Map
import GameState
import Locations
import Look
import Objects
import LocationUtils
import Endings

move :: String -> GameState -> IO GameState
move "east" state =
  if isAtLocation "train_station" state
    then do
      let message = escapeCityEndingText (hasItem "diary" state)
      putStrLn message
      let updatedState =
            state
              { gameOver = True
              }
      return updatedState
    else do
      return state

move "east" state =
  if isAtLocation "hill_church" state
    then do
      let message = hillChurchEndingEscapeText
      putStrLn message
      let updatedState =
            state
              { gameOver = True
              }
      return updatedState
    else do
      return state

move "north" state =
  if isAtLocation "forest_cave" state
    then do
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
    else do
      return state


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
goNextLocation "hill_church" state = do
    putStrLn "You stand before an old, abandoned church on the hill."
    putStrLn "The dark interior and the smell of incense remind you of ancient rituals."
    putStrLn "The priest, the last witness of the former life in the city, looks at you with an expression of concern."
    putStrLn "You can interact with him by interact(priest)"
    if hasItem "amulet" state
      then do
        putStrLn "The priest notices the amulet in your hand and warns you: That is the symbol of their cult; do not approach them with it."
        putStrLn "He continues: I can''t tell you more, just be careful"
        putStrLn "He points at the old staircase, at the west side of the church, that leads to second floor of the church"
        putStrLn "The path to the forest is to the north."
      else do
        putStrLn "The path to the forest is to the north."
    let updatedState = state {currentLocation = hillChurch}
    return updatedState

goNextLocation "archive" state = do
  putStrLn "You enter the archive room. It is small and cramped, filled with stacks of old papers and documents."
  putStrLn "Most of the documents are unreadable due to age."
  if isItemAt "green_fuse" archiveRoom
    then do
      putStrLn "However you notice a bright green fuse lying on a nearby shelf."
    else do
      putStrLn ""
  let updatedState = state {currentLocation = hillChurch}
  return updatedState

goNextLocation "parking" state = do
  putStrLn "You are in a deserted parking lot near the train station. The ground is littered"
  putStrLn "with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry."
  putStrLn "To the north, you can see a homeless man sitting on a bench."
  putStrLn "To the south is the main street,"
  putStrLn "and the train station is to the east."
  if hasItem "car_keys" state
    then do
      putStrLn "You have the car keys so you can try to open the abandoned car."
      putStrLn "Type in enter_car in order to get into the vehicle."
    else do
      putStrLn ""
  let updatedState = state {currentLocation = parking}
  return updatedState

goNextLocation "car" state = do
  putStrLn "You sit inside the car, but it refuses to start."
  putStrLn "Type in exit_car in order to get out of the vehicle."
  if isItemAt "key" car
    then do
      putStrLn "You noticed an old key lying on the passenger seat."
    else do
      putStrLn ""
  let updatedState = state {currentLocation = car}
  return updatedState


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
