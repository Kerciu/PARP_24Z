module Interactions where

import Data.Text.Array (new)
import GameState
import Interactable
import Locations
import Objects
import Utils

hasItem :: GameState -> String -> Bool
hasItem state item =
  let itemObject = findItem item
   in case itemObject of
        Nothing -> False
        Just item -> item `elem` inventory state

hasFlag :: GameState -> String -> Bool
hasFlag state flag = flag `elem` flags state

addItem :: GameState -> String -> IO GameState
addItem state itemName =
  case findItem itemName of
    Nothing -> do
      putStrLn $ "Item '" ++ itemName ++ "' does not exist."
      return state
    Just item -> do
      let updatedState = state {inventory = item : inventory state}
      return updatedState

removeItem :: GameState -> String -> IO GameState
removeItem state itemName =
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

addFlag :: GameState -> String -> IO GameState
addFlag state flag = do
  if flag `elem` flags state
    then do
      return state
    else do
      let updatedState = state {flags = flag : flags state}
      return updatedState

isAtLocation :: GameState -> String -> Bool
isAtLocation state locationString =
  case findLocation locationString of
    Nothing -> False
    Just loc -> locationName (location state) == locationName loc

give :: GameState -> String -> String -> IO GameState
give state "homeless" "cigarettes" =
  if isAtLocation state "homeless_bench" && hasItem state "cigarettes"
    then do
      putStrLn "You give the pack of cigarettes to the homeless man. He takes them eagerly and thanks you."
      putStrLn "He hands you a cold can of Harnas beer in return."
      putStrLn "And a weird box saying 'I found it a while ago, it is useless for me but maybe you can get it open'"
      newState <- addFlag state "first_homeless_interaction"
      newState <- addItem newState "weird_box"
      newState <- addItem newState "harnas"
      removeItem newState "cigarettes"
    else do
      putStrLn "You cannot give this item to that person."
      return state
give state "homeless" "harnas" =
  if isAtLocation state "homeless_bench" && hasItem state "harnas"
    then do
      putStrLn "The homeless man rejects your offer to give him Harnas beer."
      putStrLn "He already has plenty of those."
      return state
    else do
      putStrLn "You cannot give this item to that person."
      return state
give state "caretaker" "harnas" =
  if isAtLocation state "train_station" && hasItem state "harnas"
    then do
      putStrLn "You offer the Harnas beer to the caretaker. She takes it gratefully and takes a swig."
      putStrLn "'Ahh, that takes me back,' she sighs and tells you more about her story with the homeless man."
      putStrLn "Grateful, she hands you the car keys."
      newState <- addItem state "car_keys"
      removeItem newState "harnas"
    else do
      putStrLn "You cannot give this item to that person."
      return state
give state "caretaker" "cigarettes" =
  if isAtLocation state "train_station" && hasItem state "cigarettes"
    then do
      putStrLn "The caretaker rejects your offer to give her cigarettes."
      return state
    else do
      putStrLn "You cannot give this item to that person."
      return state
give state "drunkard" "kuflowe_mocne" =
  if isAtLocation state "police_station" && hasItem state "kuflowe_mocne"
    then do
      putStrLn "The drunkard takes the Kuflowe Mocne with a greedy smile."
      putStrLn "'Alright, alright... here, take this.'"
      putStrLn "He hands you a crumpled leaf with the safe code scrawled on it."
      newState <- addFlag state "second_drunkard_interaction"
      newState <- addItem newState "leaf_with_code"
      removeItem newState "kuflowe_mocne"
    else do
      putStrLn "You cannot give this item to that person."
      return state
give state _ object = do
  putStrLn "You cannot give this item to that person."
  return state

-- Homeless interaction
interactWith :: GameState -> String -> IO GameState
interactWith state "homeless" =
  if isAtLocation state "homeless_bench"
    then
      if hasItem state "cigarettes"
        then do
          putStrLn "The homeless man seems to be more interested as he coughs intensively."
          return state
        else
          if not (hasFlag state "second_homeless_interaction") && hasFlag state "first_drunkard_interaction"
            then do
              putStrLn "The homeless man looks at you knowingly."
              putStrLn "'Oh, you need a drink for ol' Bill? Here, take this Kuflowe Mocne. But don't tell him I gave it for free!'"
              newState <- addItem state "kuflowe_mocne"
              addFlag newState "second_homeless_interaction"
            else
              if hasItem state "amulet"
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
interactWith state "caretaker" =
  if isAtLocation state "train_station"
    then
      if hasItem state "amulet"
        then do
          putStrLn "The caretaker looks at you uneasily as you hold the strange amulet. 'What is that you have there?' she asks, "
          putStrLn "her eyes narrowing. 'You should be careful where you go with that.'"
          return state
        else
          if hasItem state "harnas"
            then do
              putStrLn "The caretaker seems more enticed to talk as she sees a can of cold Harnas."
              return state
            else do
              putStrLn "The caretaker seems more distant and uninterested for now."
              return state
    else do
      putStrLn "There is no caretaker here"
      return state

