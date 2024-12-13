module GameState where
import Locations
import Interactable
import Objects
import qualified Data.Map as Map

data GameState = GameState {
    currentLocation :: Location,
    inventory :: [Interactable],
    escapeCityEnding :: Bool,
    hillChurchEndingEscape :: Bool,
    forestCaveEndingKilled :: Bool,
    forestCaveEndingWeakened :: Bool,
    gameOver :: Bool,
    flags :: [String]
}

initialState :: GameState
initialState = GameState {
    currentLocation = trainStation,
    inventory = [],
    escapeCityEnding = False,
    hillChurchEndingEscape = False,
    forestCaveEndingKilled = False,
    forestCaveEndingWeakened = False,
    gameOver = False,
    flags = []
}

modifyState :: GameState -> (GameState -> GameState) -> IO ()
modifyState state f = do
    let newState = f state
    return ()

location :: GameState -> String
location state = locationName (currentLocation state)

hasItem :: GameState -> String -> Bool
hasItem state item = case findItem item of
    Just i  -> i `elem` inventory state
    Nothing -> False

getState :: GameState -> IO GameState
getState state = return state

findItem :: String -> Maybe Interactable
findItem itemName = Map.lookup itemName itemsMap
  where
    itemsMap = Map.fromList
        [ ("Cigarettes", cigarettes)
        , ("WeirdBox", weirdBox)
        , ("Harnas", harnas)
        , ("KufloweMocne", kufloweMocne)
        , ("CarKeys", carKeys)
        , ("Amulet", amulet)
        , ("Diary", diary)
        , ("Notes", notes)
        , ("Newspaper", newspaper)
        , ("LeafWithCode", leafWithCode)
        , ("EngravedRing", engravedRing)
        , ("Key", key)
        ]