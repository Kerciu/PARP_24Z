module GameState where
import Locations
import Objects
import Types

data GameState = GameState {
    location :: Location,
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
    location = trainStation,
    inventory = [],
    escapeCityEnding = False,
    hillChurchEndingEscape = False,
    forestCaveEndingKilled = False,
    forestCaveEndingWeakened = False,
    gameOver = False,
    flags = []
}