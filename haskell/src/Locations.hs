module Locations where
import Data.Map (Map)
import Objects
import Interactable
import qualified Data.Map as Map

data Direction = North | South | West | East deriving (Eq, Ord, Show)

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West = East
opposite East = West

parseDirection :: String -> Maybe Direction
parseDirection "n" = Just North
parseDirection "north" = Just North
parseDirection "s" = Just South
parseDirection "south" = Just South
parseDirection "w" = Just West
parseDirection "west" = Just West
parseDirection "e" = Just East
parseDirection "east" = Just East
parseDirection _ = Nothing


data Location = Location {
    locationName :: String,
    locationDescription :: String,
    locationItems :: [Interactable],
    locationInteractables :: [Interactable],    -- for example safe
    directions :: Map Direction (String, Bool)  -- location name and flag if is unlocked
} deriving (Show)

-- map of all locations by their names
-- type Locations = Map String Location

-- locations :: Locations
findLocation :: String -> Maybe Location
findLocation locationName = Map.lookup locationName locationsMap
  where
    locationsMap = Map.fromList
        [ ("Train Station", trainStation)
        , ("Parking", parking)
        , ("Car", car)
        , ("Homeless Bench", homelessBench)
        , ("River Tracks", riverTracks)
        , ("Main Street", mainStreet)
        , ("Old Town", oldTown)
        , ("Hotel Lobby", hotelLobby)
        , ("Hotel Toilet", hotelToilet)
        , ("Hotel Corridor", hotelCorridor)
        , ("Hotel Room", hotelRoom)
        , ("Hotel Basement", hotelBasement)
        , ("Police Station", policeStation)
        , ("Library", library)
        , ("Archive", archiveRoom)
        , ("Hill Church", hillChurch)
        , ("Second Floor of Hill Church", hillChurchSecondFloor)
        , ("Forest Cave", forestCave)
        ]

trainStation :: Location
trainStation = Location {
    locationName = "Train Station",
    locationDescription = "You are at the train station, where your adventure started." ++
    "The clock points at 3:15 am and never moves." ++
    "The timetable is written in some out-of-this-world, unintelligible language." ++
    "The only person present at the station is the caretaker." ++
    "You can interact with her by interact Caretaker" ++
    "To the west, you can see the parking area adjacent to the station." ++
    "To the east, there is a train which you can use to escape from city.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (West, ("Parking", True)),
        (East, ("Escape Train", True))
    ]
}

parking :: Location
parking = Location {
    locationName = "Parking",
    locationDescription = "You are in a deserted parking lot near the train station. The ground is littered" ++
    "with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry." ++
    "To the north, you can see a homeless man sitting on a bench." ++
    "To the south is the main street," ++
    "and the train station is to the east.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (North, ("Homeless Bench", True)),
        (South, ("Main Street", True)),
        (East, ("Train Station", True))
    ]
}

car :: Location
car = Location {
    locationName = "Car",
    locationDescription = "You sit inside the car, but it refuses to start." ++
    "Type in exit Car in order to get out of the vehicle.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.empty
}

homelessBench :: Location
homelessBench = Location {
    locationName = "Homeless Bench",
    locationDescription = "You find yourself near a bench occupied by a homeless man, muttering under his breath." ++
    "You can interact with him by interact Homeless" ++
    "The parking area lies to the south.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (South, ("Main Street", True))
    ]
}

riverTracks :: Location
riverTracks = Location {
    locationName = "River Tracks",
    locationDescription = "You stand by the river tracks. The water flows sluggishly, casting eerie reflections" ++
    "in the moonlight." ++
    "To the west, you see the main street.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (West, ("Main Street", True))
    ]
}

mainStreet :: Location
mainStreet = Location {
    locationName = "Main Street",
    locationDescription = "You are on the main street, flanked by old, abandoned shops. The cracked windows" ++
    "and faded signs give the area a ghostly feel." ++
    "To the west is the old town," ++
    "while the river tracks are to the east," ++
    "and the parking lot to the north.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (West, ("Old Town", True)),
        (East, ("River Tracks", True)),
        (North, ("Parking", True))
    ]
}

oldTown :: Location
oldTown = Location {
    locationName = "Old Town",
    locationDescription = "You find yourself in the heart of Old Town," ++
    "a desolate square filled with abandoned shops and crumbling facades." ++
    "Dust and debris cover the cobblestone streets, and a faint echo of past lives lingers in the air." ++
    "In the north there is a police station," ++
    "in the east you can see the main street." ++
    "There is an old hotel to the south" ++
    "and a library to the west.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (North, ("Police Station", True)),
        (South, ("Hotel Lobby", True)),
        (West, ("Library", True)),
        (East, ("Main Street", True))
    ]
}

hotelLobby :: Location
hotelLobby = Location {
    locationName = "Hotel Lobby",
    locationDescription = "You enter the lobby of the hotel." ++
    "There is glass scattered everywhere, and the old reception desk is covered in papers." ++
    "You can go north to return to Old Town." ++
    "There is a toilet to the east." ++
    "To the south, a dark hallway leads further into the hotel." ++
    "Looking to the west, you can see an elevator.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (North, ("Old Town", True)),
        (East, ("Hotel Toilet", True)),
        (South, ("Hotel Corridor", True)),
        (West, ("Hotel Basement", True))
    ]
}

hotelToilet :: Location
hotelToilet = Location {
    locationName = "Hotel Toilet",
    locationDescription = "You enter the toilet." ++
    "The room is dark and damp, and the smell of mold and decay fills the air." ++
    "The toilet is broken and the sink is covered in grime." ++
    "You can go west to return to the hotel lobby.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (West, ("Hotel Lobby", True))
    ]
}

hotelCorridor :: Location
hotelCorridor = Location {
    locationName = "Hotel Corridor",
    locationDescription = "You enter a dark hallway." ++
    "The ceiling has collapsed, thus most of the corridor is blocked off by rubble." ++
    "Only one room to the west remains accessible." ++
    "You can go north to return to the hotel lobby.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (North, ("Hotel Lobby", True)),
        (West, ("Hotel Room", True))
    ]
}

hotelRoom :: Location
hotelRoom = Location {
    locationName = "Hotel Room",
    locationDescription = "You enter the hotel room." ++
    "The room is dark and dusty, and the bed is covered in old sheets." ++
    "The closet is empty, and the desk is covered in papers." ++
    "You can go east to return to the corridor.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (East, ("Hotel Corridor", True))
    ]
}

hotelBasement :: Location
hotelBasement = Location {
    locationName = "Hotel Basement",
    locationDescription = "The elevator can only go down." ++
    "It leads to the basement of the hotel." ++
    "The basement is dark and damp, with a faint, musty odor filling the air." ++
    "You can go east to return to the hotel lobby.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (East, ("Hotel Lobby", True))
    ]
}

policeStation :: Location
policeStation = Location {
    locationName = "Police Station",
    locationDescription = "You enter the police station. The lights flicker, casting eerie shadows on the walls." ++
    "To the left, you see a dusty counter with an old safe behind it. " ++
    "The smell of cheap liquor and stale cigarettes fills the air." ++
    "A drunkard, with tangled hair and a worn-out jacket, stares at you with a glazed look." ++
    "He seems to be clutching an old bottle. Perhaps he knows something useful?" ++
    "You can interact with him by interact Drunkard" ++
    "You can enter the code to the safe by openSafe Code." ++
    "You can go south to return to the old town.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (South, ("Old Town", True))
    ]
}

library :: Location
library = Location {
    locationName = "Library",
    locationDescription = "You enter the library. The room is quiet, filled with towering shelves of old, dusty books." ++
    "There is the old town to the east." ++
    "To the south, you see a door leading to the archive room." ++
    "To the west, there is a church on the hill.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (East, ("Old Town", True)),
        (South, ("Archive", True)),
        (West, ("Hill Church", True))
    ]
}

archiveRoom :: Location
archiveRoom = Location {
    locationName = "Archive Room",
    locationDescription = "You enter the archive room. It is small and cramped, filled with stacks of old papers and documents." ++
    "Most of the documents are unreadable due to age.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (North, ("Library", True))
    ]
}

hillChurch :: Location
hillChurch = Location {
    locationName = "Hill Church",
    locationDescription = "You stand before an old, abandoned church on the hill." ++
    "The dark interior and the smell of incense remind you of ancient rituals." ++
    "The priest, the last witness of the former life in the city, looks at you with an expression of concern." ++
    "You can interact with him by interact Priest" ++
    "The path to the forest is to the north.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (North, ("Forest Cave", True)),
        (East, ("Library", True))
    ]
}

hillChurchSecondFloor :: Location
hillChurchSecondFloor = Location {
    locationName = "Second Floor of Hill Church",
    locationDescription = "You are now at the second floor of the church." ++
    "You see some kind of weird numbers that seem out of order at the wall" ++
    "4 6 1 2 6 7 3 4 1 5 6 2 7 3 5 7 3 2 5 3 6 4 3 6 7 2 " ++
    "You notice also a sentence:\"That's a code to the truth of this mystery.\"" ++
    "To go back to first floor go east",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (East, ("Hill Church", True))
    ]
}

forestCave :: Location
forestCave = Location {
    locationName = "Forest Cave",
    locationDescription = "You enter a dark cave hidden deep in the forest." ++
    "In the center of the cave stands an altar with a strange symbol." ++
    "The symbol looks familiar, it might be an ancient artifact sought by the archaeologists." ++
    "Here you find evidence that the cult still exists and conducts its rituals here." ++
    "You feel that this place may be key to solving the mystery of the archaeologists’ disappearance." ++
    "You see far far to the north of this huge cave a weird doors that must lead to something." ++
    "The path back leads south, returning to the church.",
    locationItems = [],
    locationInteractables = [],
    directions = Map.fromList [
        (South, ("Hill Church", True)),
        (North, ("Ending Cave", True))
    ]
}
