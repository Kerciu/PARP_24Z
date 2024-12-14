module Interactions where

import Data.Text.Array (new)
import GameState
import Interactable
import Locations
import Objects
import Utils

hasItem :: String -> GameState -> Bool
hasItem item state =
  let itemObject = findItem item
   in case itemObject of
        Nothing -> False
        Just item -> item `elem` inventory state

hasFlag :: String -> GameState -> Bool
hasFlag flag state = flag `elem` flags state

addItem :: String -> GameState -> IO GameState
addItem itemName state =
  case findItem itemName of
    Nothing -> do
      putStrLn $ "Item '" ++ itemName ++ "' does not exist."
      return state
    Just item -> do
      let updatedState = state {inventory = item : inventory state}
      return updatedState

removeItem :: String -> GameState -> IO GameState
removeItem itemName state =
  case findItem itemName of
    Nothing -> do
      putStrLn $ "Item '" ++ itemName ++ "' does not exist."
      return state
    Just item ->
      if item `elem` inventory state
        then do
          let updatedState = state {inventory = filter (/= item) (inventory state)}
          return updatedState
        else do
          return state

addFlag :: String -> GameState -> IO GameState
addFlag flag state = do
  if flag `elem` flags state
    then do
      return state
    else do
      let updatedState = state {flags = flag : flags state}
      return updatedState

isAtLocation :: String -> GameState -> Bool
isAtLocation locationString state =
  case findLocation locationString of
    Nothing -> False
    Just loc -> locationName (currentLocation state) == locationName loc

open :: String -> GameState -> IO GameState
open "weird_box" state =
  if hasItem "weird_box" state
    then
      if isAtLocation "second_floor_of_hill_church" state
        then do
          putStrLn "You open the box with the code and find weird notes inside."
          putStrLn "One of those says: 'U CANNOT DEFEAT THEM FLEE FROM THE CITY AS FAST AS YOU CAN, THEY ARE CLOSING UP ON ME. THIS IS MY FAREWELL, GOODBYE THE ONE THAT READ THOOSE'."
          putStrLn "You are now able to decide, whether to continue your journey or run away from the city."
          putStrLn "Decide fast or maybe you will meet the same fate as a writer of thoose notes."
          putStrLn "To flee from the city to train station you have to go north for a secret passage in the church."
          putStrLn "To return to the first floor of the church go east."
          addFlag "weird_box_opened" state
        else do
          putStrLn "You have to know a 20-digit code to open this box."
          putStrLn "Maybe you can find it somewhere."
          return state
    else do
      putStrLn "You don't have weird_box in your inventory."
      return state
open "safe" state =
  if isAtLocation "police_station" state
    then
      if hasFlag "safe_opened" state
        then do
          putStrLn "The safe is already opened."
          return state
        else do
          putStrLn "You have to enter 4-digit code to open the safe:"
          code <- getLine
          if code == "2137"
            then do
              putStrLn "The safe opens, revealing an engraved ring inside."
              putStrLn "You take the engraved ring."
              newState <- addItem "engraved_ring" state
              addFlag "safe_opened" newState
            else do
              putStrLn "Wrong code!"
              return state
    else do
      putStrLn "There is no safe here."
      return state
open _ state = do
  putStrLn "You cannot open this item."
  return state

give :: String -> String -> GameState -> IO GameState
give "homeless" "cigarettes" state =
  if isAtLocation "homeless_bench" state && hasItem "cigarettes" state
    then do
      putStrLn "You give the pack of cigarettes to the homeless man. He takes them eagerly and thanks you."
      putStrLn "He hands you a cold can of Harnas beer in return."
      putStrLn "And a weird box saying 'I found it a while ago, it is useless for me but maybe you can get it open'"
      newState <- addFlag "first_homeless_interaction" state
      newState <- addItem "weird_box" newState
      newState <- addItem "harnas" newState
      removeItem "cigarettes" newState
    else do
      putStrLn "You cannot give this item to that person."
      return state
give "homeless" "harnas" state =
  if isAtLocation "homeless_bench" state && hasItem "harnas" state
    then do
      putStrLn "The homeless man rejects your offer to give him Harnas beer."
      putStrLn "He already has plenty of those."
      return state
    else do
      putStrLn "You cannot give this item to that person."
      return state
give "caretaker" "harnas" state =
  if isAtLocation "train_station" state && hasItem "harnas" state
    then do
      putStrLn "You offer the Harnas beer to the caretaker. She takes it gratefully and takes a swig."
      putStrLn "'Ahh, that takes me back,' she sighs and tells you more about her story with the homeless man."
      putStrLn "Grateful, she hands you the car keys."
      newState <- addItem "car_keys" state
      removeItem "harnas" newState
    else do
      putStrLn "You cannot give this item to that person."
      return state
give "caretaker" "cigarettes" state =
  if isAtLocation "train_station" state && hasItem "cigarettes" state
    then do
      putStrLn "The caretaker rejects your offer to give her cigarettes."
      return state
    else do
      putStrLn "You cannot give this item to that person."
      return state
give "drunkard" "kuflowe_mocne" state =
  if isAtLocation "police_station" state && hasItem "kuflowe_mocne" state
    then do
      putStrLn "The drunkard takes the Kuflowe Mocne with a greedy smile."
      putStrLn "'Alright, alright... here, take this.'"
      putStrLn "He hands you a crumpled leaf with the safe code scrawled on it."
      newState <- addFlag "second_drunkard_interaction" state
      newState <- addItem "leaf_with_code" newState
      removeItem "kuflowe_mocne" newState
    else do
      putStrLn "You cannot give this item to that person."
      return state
give _ _ state = do
  putStrLn "You cannot give this item to that person."
  return state

-- Homeless interaction
interactWith :: String -> GameState -> IO GameState
interactWith "homeless" state =
  if isAtLocation "homeless_bench" state
    then
      if hasItem "cigarettes" state
        then do
          putStrLn "The homeless man seems to be more interested as he coughs intensively."
          return state
        else
          if not (hasFlag "second_homeless_interaction" state) && hasFlag "first_drunkard_interaction" state
            then do
              putStrLn "The homeless man looks at you knowingly."
              putStrLn "'Oh, you need a drink for ol' Bill? Here, take this Kuflowe Mocne. But don't tell him I gave it for free!'"
              newState <- addItem "kuflowe_mocne" state
              addFlag "second_homeless_interaction" newState
            else
              if hasItem "amulet" state
                then do
                  putStrLn "The homeless man looks at you with wide eyes. 'You hold that cursed thing,' he says, "
                  putStrLn "his voice shaking. 'I feel something watching... beware.'"
                  return state
                else do
                  putStrLn "The homeless man seems to be uninterested for now."
                  return state
    else do
      putStrLn "There is no homeless man here"
      return state

-- Caretaker interaction
interactWith "caretaker" state =
  if isAtLocation "train_station" state
    then
      if hasItem "amulet" state
        then do
          putStrLn "The caretaker looks at you uneasily as you hold the strange amulet. 'What is that you have there?' she asks, "
          putStrLn "her eyes narrowing. 'You should be careful where you go with that.'"
          return state
        else
          if hasItem "harnas" state
            then do
              putStrLn "The caretaker seems more enticed to talk as she sees a can of cold Harnas."
              return state
            else do
              putStrLn "The caretaker seems more distant and uninterested for now."
              return state
    else do
      putStrLn "There is no caretaker here"
      return state

-- Drunkard interaction
interactWith "drunkard" state =
  if isAtLocation "police_station" state
    then
      if not (hasFlag "second_drunkard_interaction" state)
        then do
          putStrLn "The drunkard looks at you with a smirk."
          putStrLn "'Lookin' for the safe code, eh? I might remember it... But I'm real thirsty.'"
          putStrLn "'Maybe if you bring me something to drink, I'll let you in on the secret.'"
          addFlag "first_drunkard_interaction" state
        else do
          putStrLn "I've already helped you, now let me enjoy my beer."
          return state
    else do
      putStrLn "There is no drunkard here"
      return state

-- Priest interaction
interactWith "priest" state =
  if isAtLocation "hill_church" state
    then do
      putStrLn "I can't tell you more, just go."
      return state
    else do
      putStrLn "There is no priest here"
      return state

-- No interaction
interactWith person state = do
  putStrLn $ "There is no " ++ person ++ " here"
  return state
