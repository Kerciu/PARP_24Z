module Look where

import Data.Map qualified as Map
import GameState
import Interactable
import Locations
import Utils

lookAround :: GameState -> IO GameState
lookAround state = do
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
