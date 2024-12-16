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
    inventory = [],
    locationItems = Map.fromList
        [ ("hotel_basement", [notes]),
          ("car", [key]),
          ("hotel_basement", [amulet]),
          ("hotel_room", [diary]),
          ("hotel_room",[blueFuse]),
          ("hotel_toilet", [redFuse]),
          ("hotel_basement", [notes]),
          ("library", [newspaper]),
          ("river_tracks", [cigarettes]),
          ("archive", [greenFuse])
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
