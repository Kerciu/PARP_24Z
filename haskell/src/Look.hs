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
    "homeless_bench" -> do
      putStrLn "You find yourself near a bench occupied by a homeless man, muttering under his breath."
      if "first_homeless_interaction" `notElem` flags state
        then do
          putStrLn "He warns of the 'shadows that follow at night' and clutches an old bottle with"
          putStrLn "You notice that he cannot breathe properly, he probably ran out of cigarettes."
        else do
          putStr ""
      if cigarettes `elem` inventory state
        then do
          putStrLn "You notice that you can help the homeless man with a cigarette."
        else do
          putStr ""
      putStrLn "You can interact with him by interact homeless"
      putStrLn "The parking area lies to the south."
    "hotel_basement" -> do
      putStrLn "The elevator can only go down."
      putStrLn "It leads to the basement of the hotel."
      putStrLn "The basement is dark and damp, with a faint, musty odor filling the air."
      if isItemAt amulet "hotel_basement" state
        then do
          putStrLn "Among the piles of old crates and broken furniture, something stands out -"
          putStrLn "a strange amulet positioned in the center of the room."
        else do
          putStr ""
      if isItemAt notes "hotel_basement" state
        then do
          putStrLn "You notice a set of old, crumbling notes scattered across a dusty table."
        else do
          putStr ""
      putStrLn "You can go east to return to the hotel lobby."
    "archive" -> do
      putStrLn "You enter the archive room. It is small and cramped, filled with stacks of old papers and documents."
      putStrLn "Most of the documents are unreadable due to age."
      if isItemAt greenFuse "archive" state
        then do
          putStrLn "However you notice a bright green fuse lying on a nearby shelf."
        else do
          putStr ""
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
          putStr ""
    "car" -> do
      putStrLn "You sit inside the car, but it refuses to start."
      putStrLn "Type in exit_car in order to get out of the vehicle."
      if isItemAt key "car" state
        then do
          putStrLn "You noticed an old key lying on the passenger seat."
        else do
          putStr ""
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
