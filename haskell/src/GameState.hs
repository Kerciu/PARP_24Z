module GameState where
import Locations
import Objects
import Interactable
import qualified Data.Map as Map
import Text.XHtml (archive)

data GameState = GameState {
    currentLocation :: Location,
    inventory :: [Interactable],
    locationItems :: Map.Map String [Interactable],
    closedLocations :: [String],
    escapeCityEnding :: Bool,
    hillChurchEndingEscape :: Bool,
    forestCaveEndingKilled :: Bool,
    forestCaveEndingWeakened :: Bool,
    gameOver :: Bool,
    flags :: [String]
}

initialState :: GameState
initialState = GameState {
    currentLocation = hotelLobby,
    inventory = [redFuse, blueFuse, greenFuse],
    locationItems = Map.fromList
        [ ("train_station", [notes]),
          ("parking", [newspaper])
        ],
    closedLocations = [
        "hotel_room",
        "hotel_basement",
        "archive"
    ],
    escapeCityEnding = False,
    hillChurchEndingEscape = False,
    forestCaveEndingKilled = False,
    forestCaveEndingWeakened = False,
    gameOver = False,
    flags = []
}
