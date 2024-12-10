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
