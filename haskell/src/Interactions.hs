module Interactions where
import GameState

import Control.Monad (when)
import Data.Maybe (fromMaybe)

hasFlag :: GameState -> String -> Bool
hasFlag state flag = flag `elem` flags state

addItem :: GameState -> String -> IO ()
addItem game itemName = case findItem itemName of
    Just item -> modifyState game $ \state -> state { inventory = item : inventory state }
    Nothing   -> putStrLn ("Item " ++ itemName ++ " not found.")

removeItem :: GameState -> String -> IO ()
removeItem game itemName = case findItem itemName of
    Just item -> modifyState game $ \state -> state { inventory = filter (/= item) (inventory state) }
    Nothing   -> putStrLn ("Item " ++ itemName ++ " not found.")


addFlag :: GameState -> String -> IO ()
addFlag game flag = modifyState game $ \state -> state { flags = flag : flags state }

give :: GameState -> String -> String -> IO ()
give state "homeless" "cigarettes" = do
    when (location state == "homeless_bench" && hasItem state "cigarettes") $ do
        putStrLn "You give the pack of cigarettes to the homeless man. He takes them eagerly and thanks you."
        putStrLn "He hands you a cold can of Harnas beer in return."
        putStrLn "And a weird box saying 'I found it a while ago, it is useless for me but maybe you can get it open'"
        addFlag state "first_homeless_interaction"
        addItem state "weird_box"
        addItem state "harnas"
        removeItem state "cigarettes"
give state "homeless" "harnas" = do
    when (location state == "homeless_bench" && hasItem state "harnas") $ do
        putStrLn "The homeless man rejects your offer to give him Harnas beer."
        putStrLn "He already has plenty of those."
give state "caretaker" "harnas" = do
    when (location state == "train_station" && hasItem state "harnas") $ do
        putStrLn "You offer the Harnas beer to the caretaker. She takes it gratefully and takes a swig."
        putStrLn "'Ahh, that takes me back,' she sighs and tells you more about her story with the homeless man."
        putStrLn "Grateful, she hands you the car keys."
        addItem state "car_keys"
        removeItem state "harnas"
give state "caretaker" "cigarettes" = do
    when (location state == "train_station" && hasItem state "cigarettes") $ do
        putStrLn "The caretaker rejects your offer to give her cigarettes."
give state "drunkard" "kuflowe_mocne" = do
    when (location state == "police_station" && hasItem state "kuflowe_mocne") $ do
        putStrLn "The drunkard takes the Kuflowe Mocne with a greedy smile."
        putStrLn "'Alright, alright... here, take this.'"
        putStrLn "He hands you a crumpled leaf with the safe code scrawled on it."
        addItem state "leaf_with_code"
        removeItem state "kuflowe_mocne"
give game _ object = do
    state <- getState game
    if not (hasItem state object)
        then putStrLn "You don't have that."
        else putStrLn "You can't give that to that person."

interactWith :: GameState -> String -> IO ()
interactWith state "homeless" = do
    case location state of
        "homeless_bench" | hasItem state "cigarettes" ->
            putStrLn "The homeless man seems to be more interested as he coughs intensively."
        "homeless_bench" | not (hasFlag state "second_homeless_interaction") -> do
            putStrLn "The homeless man looks at you knowingly."
            putStrLn "'Oh, you need a drink for ol' Bill? Here, take this Kuflowe Mocne. But don't tell him I gave it for free!'"
            addItem state "kuflowe_mocne"
            addFlag state "second_homeless_interaction"
        "homeless_bench" | hasItem state "amulet" -> do
            putStrLn "The homeless man looks at you with wide eyes. 'You hold that cursed thing,' he says, "
            putStrLn "his voice shaking. 'I feel something watching... beware.'"
        "homeless_bench" ->
            putStrLn "The homeless man seems to be uninterested for now."
        _ -> return ()
interactWith state "caretaker" = do
    case location state of
        "train_station" | hasItem state "amulet" -> do
            putStrLn "The caretaker looks at you uneasily as you hold the strange amulet. 'What is that you have there?' she asks, "
            putStrLn "her eyes narrowing. 'You should be careful where you go with that.'"
        "train_station" ->
            putStrLn "The caretaker seems more distant and uninterested for now."
        _ -> return ()
interactWith state "drunkard" = do
    when (location state == "police_station") $ do
        putStrLn "The drunkard looks at you with a smirk."
        putStrLn "'Lookin' for the safe code, eh? I might remember it... But I'm real thirsty.'"
        putStrLn "'Maybe if you bring me something to drink, I'll let you in on the secret.'"
        addFlag state "drunkard_interaction"
interactWith state "priest" = do
    when (location state == "hill_church") $ do
        putStrLn "I can't tell you more, just go."