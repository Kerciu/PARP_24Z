module Look where

import Data.Map qualified as Map
import GameState
import Interactable
import LocationUtils
import Locations
import Objects
import Utils

lookAround :: GameState -> IO GameState
lookAround state = do
  case locationName (currentLocation state) of
    "archive" -> do
      putStrLn "You enter the archive room. It is small and cramped, filled with stacks of old papers and documents."
      putStrLn "Most of the documents are unreadable due to age."
      if isItemAt greenFuse "archive" state
        then do
          putStrLn "However you notice a bright green fuse lying on a nearby shelf."
        else do
          putStrLn ""
    "parking" -> do
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
    "car" -> do
      putStrLn "You sit inside the car, but it refuses to start."
      putStrLn "Type in exit_car in order to get out of the vehicle."
      if isItemAt key "car" state
        then do
          putStrLn "You noticed an old key lying on the passenger seat."
        else do
          putStrLn ""
    _ -> do
      printLines [locationDescription (currentLocation state)]

  let locationString = locationName (currentLocation state)

  case Map.lookup locationString (locationItems state) of
    Nothing -> return ()
    Just items ->
      case items of
        [] -> return ()
        [item] -> putStrLn $ "You see " ++ name item ++ " here."
        _ -> do
          putStrLn "You see these items:"
          mapM_ (putStrLn . name) items

  return state
