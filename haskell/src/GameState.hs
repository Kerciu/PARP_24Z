module GameState where
import Locations
import Objects
import Interactable
import qualified Data.Map as Map

data GameState = GameState {
    currentLocation :: Location,
    inventory :: [Interactable],
    locations :: Map.Map String Location,
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
    inventory = [amulet],
    locations = Map.fromList [
        ("train_station", trainStation),
        ("parking", parking),
        ("car", car),
        ("homeless_bench", homelessBench),
        ("river_tracks", riverTracks),
        ("main_street", mainStreet),
        ("old_town", oldTown),
        ("hotel_lobby", hotelLobby),
        ("hotel_toilet", hotelToilet),
        ("hotel_corridor", hotelCorridor),
        ("hotel_room", hotelRoom),
        ("hotel_basement", hotelBasement),
        ("police_station", policeStation),
        ("library", library),
        ("archive", archiveRoom),
        ("hill_church", hillChurch),
        ("second_floor_of_hill_church", hillChurchSecondFloor),
        ("forest_cave", forestCave)
    ],
    escapeCityEnding = False,
    hillChurchEndingEscape = False,
    forestCaveEndingKilled = False,
    forestCaveEndingWeakened = False,
    gameOver = False,
    flags = []
}
