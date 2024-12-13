module Interactions where

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
      newState <- addItem state "leaf_with_code"
      removeItem newState "kuflowe_mocne"
    else do
      putStrLn "You cannot give this item to that person."
      return state
give state _ object = do
  putStrLn "You cannot give this item to that person."
  return state

interactWith :: GameState -> String -> IO GameState
interactWith state "homeless" = do
  if isAtLocation state "homeless_bench"
    then do
      if hasItem state "cigarettes"
        then do
          putStrLn "The homeless man seems to be more interested as he coughs intensively."
          return state
        else
          if not (hasFlag state "second_homeless_interaction") && hasFlag state "drunkard_interaction"
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
      putStrLn "There is no homeless here"
      return state

-- "homeless_bench"
--   | hasItem state "cigarettes" ->
--       putStrLn "The homeless man seems to be more interested as he coughs intensively."
-- "homeless_bench" | not (hasFlag state "second_homeless_interaction") -> do
--   putStrLn "The homeless man looks at you knowingly."
--   putStrLn "'Oh, you need a drink for ol' Bill? Here, take this Kuflowe Mocne. But don't tell him I gave it for free!'"
--   addItem state "kuflowe_mocne"
--   addFlag state "second_homeless_interaction"
-- "homeless_bench" | hasItem state "amulet" -> do
--   putStrLn "The homeless man looks at you with wide eyes. 'You hold that cursed thing,' he says, "
--   putStrLn "his voice shaking. 'I feel something watching... beware.'"
-- "homeless_bench" ->
--   putStrLn "The homeless man seems to be uninterested for now."
-- _ -> return ()

-- interactWith state "homeless" = do
--   case locationName (location state) of
--     "homeless_bench"
--       | hasItem state "cigarettes" ->
--           putStrLn "The homeless man seems to be more interested as he coughs intensively."
--     "homeless_bench" | not (hasFlag state "second_homeless_interaction") -> do
--       putStrLn "The homeless man looks at you knowingly."
--       putStrLn "'Oh, you need a drink for ol' Bill? Here, take this Kuflowe Mocne. But don't tell him I gave it for free!'"
--       addItem state "kuflowe_mocne"
--       addFlag state "second_homeless_interaction"
--     "homeless_bench" | hasItem state "amulet" -> do
--       putStrLn "The homeless man looks at you with wide eyes. 'You hold that cursed thing,' he says, "
--       putStrLn "his voice shaking. 'I feel something watching... beware.'"
--     "homeless_bench" ->
--       putStrLn "The homeless man seems to be uninterested for now."
--     _ -> return ()
-- interactWith state "caretaker" = do
--   case location state of
--     "train_station" | hasItem state "amulet" -> do
--       putStrLn "The caretaker looks at you uneasily as you hold the strange amulet. 'What is that you have there?' she asks, "
--       putStrLn "her eyes narrowing. 'You should be careful where you go with that.'"
--     "train_station" ->
--       putStrLn "The caretaker seems more distant and uninterested for now."
--     _ -> return ()
-- interactWith state "drunkard" = do
--   when (location state == "police_station") $ do
--     putStrLn "The drunkard looks at you with a smirk."
--     putStrLn "'Lookin' for the safe code, eh? I might remember it... But I'm real thirsty.'"
--     putStrLn "'Maybe if you bring me something to drink, I'll let you in on the secret.'"
--     addFlag state "drunkard_interaction"
-- interactWith state "priest" = do
--   when (location state == "hill_church") $ do
--     putStrLn "I can't tell you more, just go."
