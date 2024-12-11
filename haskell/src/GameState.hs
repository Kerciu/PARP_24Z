module GameState where
import Locations
import Interactable

data GameState = GameState {
    currentLocation :: Location,
    inventory :: [Interactable],
    drunkardInteraction :: Bool,
    firstHomelessInteraction :: Bool,
    secondHomelessInteraction :: Bool,
    escapeCityEnding :: Bool,
    hillChurchEndingEscape :: Bool,
    forestCaveEndingKilled :: Bool,
    forestCaveEndingWeakened :: Bool,
    gameOver :: Bool
}

initialState :: GameState
initialState = GameState {
    currentLocation = trainStation,
    inventory = [],
    drunkardInteraction = False,
    firstHomelessInteraction = False,
    secondHomelessInteraction = False,
    escapeCityEnding = False,
    hillChurchEndingEscape = False,
    forestCaveEndingKilled = False,
    forestCaveEndingWeakened = False,
    gameOver = False
}