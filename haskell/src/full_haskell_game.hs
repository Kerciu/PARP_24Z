
module Main where

import Data.List (intercalate)
import Control.Monad.State

-- Data Types for Game Entities
data Location = TrainStation | ParkingLot | Car | HomelessBench | RiverTracks
              | MainStreet | OldTown | HotelLobby | HotelToilet | HotelCorridor
              | HotelRoom | HotelBasement | PoliceStation | Library | Archive
              | HillChurch | HillChurchSecondFloor | ForestCave | EndingCave
              deriving (Eq, Show, Read)

data Object = Cigarettes | Amulet | Diary | RedFuse | BlueFuse
            | GreenFuse | Key | CarKeys | EngravedRing
            deriving (Eq, Show, Read)

data GameState = GameState
    { playerLocation :: Location
    , inventory      :: [Object]
    , unlockedRooms  :: [Location]
    } deriving Show

-- Initial Game State
initialState :: GameState
initialState = GameState
    { playerLocation = TrainStation
    , inventory = []
    , unlockedRooms = [TrainStation, ParkingLot]
    }

-- Descriptions of Locations
describe :: Location -> String
describe TrainStation = "You are at the train station. The city lies to the west."
describe ParkingLot = "You are in the parking lot. An abandoned car is here."
describe Car = "You are in the car. It won't start."
describe HomelessBench = "You see a homeless man on a bench. The main street is to the south."
describe RiverTracks = "You are by the river tracks. The water flows sluggishly."
describe MainStreet = "You are on the main street, surrounded by old shops."
describe OldTown = "You are in Old Town, a desolate square with crumbling buildings."
describe HotelLobby = "You are in the hotel lobby. It's filled with glass and papers."
describe HotelToilet = "You are in the hotel toilet. It smells of mold and decay."
describe HotelCorridor = "You are in a dark hotel corridor, with one accessible room."
describe HotelRoom = "You are in the hotel room. A diary lies on the desk."
describe HotelBasement = "You are in the hotel basement. An amulet is here."
describe PoliceStation = "You are in the police station. A drunkard sits at the counter."
describe Library = "You are in the library. Old books fill the shelves."
describe Archive = "You are in the archive. A green fuse lies on a shelf."
describe HillChurch = "You are at the hill church. The priest looks at you."
describe HillChurchSecondFloor = "You are on the second floor of the church. Strange numbers are on the wall."
describe ForestCave = "You are in a dark cave. An altar stands in the center."
describe EndingCave = "You are in the ending cave. Something ancient is here."

-- Movement and Restrictions
move :: Location -> GameState -> GameState
move newLocation state
    | newLocation `elem` unlockedRooms state = state { playerLocation = newLocation }
    | otherwise = state

unlockRoom :: Location -> GameState -> GameState
unlockRoom room state = state { unlockedRooms = room : unlockedRooms state }

-- Interactions
pickUp :: Object -> GameState -> GameState
pickUp obj state = state { inventory = obj : inventory state }

dropObject :: Object -> GameState -> GameState
dropObject obj state = state { inventory = filter (/= obj) (inventory state) }

hasObject :: Object -> GameState -> Bool
hasObject obj state = obj `elem` inventory state

-- Game Loop
gameLoop :: StateT GameState IO ()
gameLoop = do
    state <- get
    liftIO $ putStrLn $ "
You are at: " ++ show (playerLocation state)
    liftIO $ putStrLn $ describe (playerLocation state)
    liftIO $ putStrLn "What would you like to do?"
    liftIO $ putStrLn "1. Move"
    liftIO $ putStrLn "2. Pick up object"
    liftIO $ putStrLn "3. Drop object"
    liftIO $ putStrLn "4. Check inventory"
    choice <- liftIO getLine
    case choice of
        "1" -> do
            liftIO $ putStrLn "Where would you like to go?"
            loc <- liftIO getLine
            let newLocation = read loc :: Location
            if newLocation `elem` unlockedRooms state
                then put $ move newLocation state
                else liftIO $ putStrLn "That location is locked or unavailable."
        "2" -> do
            liftIO $ putStrLn "What would you like to pick up?"
            obj <- liftIO getLine
            let object = read obj :: Object
            put $ pickUp object state
        "3" -> do
            liftIO $ putStrLn "What would you like to drop?"
            obj <- liftIO getLine
            let object = read obj :: Object
            put $ dropObject object state
        "4" -> do
            liftIO $ putStrLn $ "You have: " ++ show (inventory state)
        _ -> liftIO $ putStrLn "Invalid choice."
    gameLoop

-- Main Function
main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Adventure Game!"
    evalStateT gameLoop initialState
