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
    hillChurchEndingEscape :: Bool,
    gameOver :: Bool,
    flags :: [String]
}

initialState :: GameState
initialState = GameState {
    currentLocation = trainStation,
    inventory = [carKeys],
    locationItems = Map.fromList
        [ ("hotel_basement", [notes]),
          ("car", [key]),
          ("hotel_basement", [amulet])
        ],
    closedLocations = [
        "hotel_room",
        "hotel_basement",
        "archive"
    ],
    hillChurchEndingEscape = False,
    gameOver = False,
    flags = []
}
