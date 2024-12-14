module Locations where

import Data.Map (Map)
import Data.Map qualified as Map
import Interactable
import Objects

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

data Location = Location
  { locationName :: String,
    locationDescription :: String,
    locationItems :: [Interactable],
    locationInteractables :: [Interactable], -- for example safe
    directions :: Map Direction (String, Bool) -- location name and flag if is unlocked
  }
  deriving (Show)

findLocation :: String -> Maybe Location
findLocation locationName = Map.lookup locationName locationsMap
  where
    locationsMap =
      Map.fromList
        [ ("train_station", trainStation),
          ("parking", parking),
          ("car", car),
          ("homeless_bench", homelessBench),
          ("river_tracks", riverTracks),
          ("main_street", mainStreet),
          ("old_town", oldTown),
          ("hotel_lobby", hotelLobby),
          ("hotel_toilet", hotelToilet),
          ("hotel_rorridor", hotelCorridor),
          ("hotel_room", hotelRoom),
          ("hotel_basement", hotelBasement),
          ("police_station", policeStation),
          ("library", library),
          ("archive", archiveRoom),
          ("hill_church", hillChurch),
          ("second_floor_of_hill_church", hillChurchSecondFloor),
          ("forest_cave", forestCave)
        ]

trainStation :: Location
trainStation =
  Location
    { locationName = "train_station",
      locationDescription =
        "You are at the train station, where your adventure started."
          ++ "The clock points at 3:15 am and never moves."
          ++ "The timetable is written in some out-of-this-world, unintelligible language."
          ++ "The only person present at the station is the caretaker."
          ++ "You can interact with her by interact Caretaker"
          ++ "To the west, you can see the parking area adjacent to the station."
          ++ "To the east, there is a train which you can use to escape from city.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (West, ("parking", True)),
            (East, ("escape_train", True))
          ]
    }

parking :: Location
parking =
  Location
    { locationName = "parking",
      locationDescription =
        "You are in a deserted parking lot near the train station. The ground is littered"
          ++ "with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry."
          ++ "To the north, you can see a homeless man sitting on a bench."
          ++ "To the south is the main street,"
          ++ "and the train station is to the east.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (North, ("homeless_bench", True)),
            (South, ("main_street", True)),
            (East, ("train_station", True))
          ]
    }

car :: Location
car =
  Location
    { locationName = "car",
      locationDescription =
        "You sit inside the car, but it refuses to start."
          ++ "Type in exit Car in order to get out of the vehicle.",
      locationItems = [],
      locationInteractables = [],
      directions = Map.empty
    }

homelessBench :: Location
homelessBench =
  Location
    { locationName = "homeless_bench",
      locationDescription =
        "You find yourself near a bench occupied by a homeless man, muttering under his breath."
          ++ "You can interact with him by interact Homeless"
          ++ "The parking area lies to the south.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (South, ("parking", True))
          ]
    }

riverTracks :: Location
riverTracks =
  Location
    { locationName = "river_tracks",
      locationDescription =
        "You stand by the river tracks. The water flows sluggishly, casting eerie reflections"
          ++ "in the moonlight."
          ++ "To the west, you see the main street.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (West, ("main_street", True))
          ]
    }

mainStreet :: Location
mainStreet =
  Location
    { locationName = "main_street",
      locationDescription =
        "You are on the main street, flanked by old, abandoned shops. The cracked windows"
          ++ "and faded signs give the area a ghostly feel."
          ++ "To the west is the old town,"
          ++ "while the river tracks are to the east,"
          ++ "and the parking lot to the north.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (West, ("old_town", True)),
            (East, ("river_tracks", True)),
            (North, ("parking", True))
          ]
    }

oldTown :: Location
oldTown =
  Location
    { locationName = "old_town",
      locationDescription =
        "You find yourself in the heart of Old Town,"
          ++ "a desolate square filled with abandoned shops and crumbling facades."
          ++ "Dust and debris cover the cobblestone streets, and a faint echo of past lives lingers in the air."
          ++ "In the north there is a police station,"
          ++ "in the east you can see the main street."
          ++ "There is an old hotel to the south"
          ++ "and a library to the west.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (North, ("police_station", True)),
            (South, ("hotel_lobby", True)),
            (West, ("library", True)),
            (East, ("main_street", True))
          ]
    }

hotelLobby :: Location
hotelLobby =
  Location
    { locationName = "hotel_lobby",
      locationDescription =
        "You enter the lobby of the hotel."
          ++ "There is glass scattered everywhere, and the old reception desk is covered in papers."
          ++ "You can go north to return to Old Town."
          ++ "There is a toilet to the east."
          ++ "To the south, a dark hallway leads further into the hotel."
          ++ "Looking to the west, you can see an elevator.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (North, ("old_town", True)),
            (East, ("hotel_toilet", True)),
            (South, ("hotel_corridor", True)),
            (West, ("hotel_basement", True))
          ]
    }

hotelToilet :: Location
hotelToilet =
  Location
    { locationName = "hotel_toilet",
      locationDescription =
        "You enter the toilet."
          ++ "The room is dark and damp, and the smell of mold and decay fills the air."
          ++ "The toilet is broken and the sink is covered in grime."
          ++ "You can go west to return to the hotel lobby.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (West, ("hotel_lobby", True))
          ]
    }

hotelCorridor :: Location
hotelCorridor =
  Location
    { locationName = "hotel_corridor",
      locationDescription =
        "You enter a dark hallway."
          ++ "The ceiling has collapsed, thus most of the corridor is blocked off by rubble."
          ++ "Only one room to the west remains accessible."
          ++ "You can go north to return to the hotel lobby.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (North, ("hotel_lobby", True)),
            (West, ("hotel_room", True))
          ]
    }

hotelRoom :: Location
hotelRoom =
  Location
    { locationName = "hotel_room",
      locationDescription =
        "You enter the hotel room."
          ++ "The room is dark and dusty, and the bed is covered in old sheets."
          ++ "The closet is empty, and the desk is covered in papers."
          ++ "You can go east to return to the corridor.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (East, ("hotel_corridor", True))
          ]
    }

hotelBasement :: Location
hotelBasement =
  Location
    { locationName = "hotel_basement",
      locationDescription =
        "The elevator can only go down."
          ++ "It leads to the basement of the hotel."
          ++ "The basement is dark and damp, with a faint, musty odor filling the air."
          ++ "You can go east to return to the hotel lobby.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (East, ("hotel_lobby", True))
          ]
    }

policeStation :: Location
policeStation =
  Location
    { locationName = "police_station",
      locationDescription =
        "You enter the police station. The lights flicker, casting eerie shadows on the walls."
          ++ "To the left, you see a dusty counter with an old safe behind it. "
          ++ "The smell of cheap liquor and stale cigarettes fills the air."
          ++ "A drunkard, with tangled hair and a worn-out jacket, stares at you with a glazed look."
          ++ "He seems to be clutching an old bottle. Perhaps he knows something useful?"
          ++ "You can interact with him by interact Drunkard"
          ++ "You can enter the code to the safe by openSafe Code."
          ++ "You can go south to return to the old town.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (South, ("old_town", True))
          ]
    }

library :: Location
library =
  Location
    { locationName = "library",
      locationDescription =
        "You enter the library. The room is quiet, filled with towering shelves of old, dusty books."
          ++ "There is the old town to the east."
          ++ "To the south, you see a door leading to the archive room."
          ++ "To the west, there is a church on the hill.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (East, ("old_town", True)),
            (South, ("archive", True)),
            (West, ("hill_church", True))
          ]
    }

archiveRoom :: Location
archiveRoom =
  Location
    { locationName = "archive_room",
      locationDescription =
        "You enter the archive room. It is small and cramped, filled with stacks of old papers and documents."
          ++ "Most of the documents are unreadable due to age.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (North, ("library", True))
          ]
    }

hillChurch :: Location
hillChurch =
  Location
    { locationName = "hill_church",
      locationDescription =
        "You stand before an old, abandoned church on the hill."
          ++ "The dark interior and the smell of incense remind you of ancient rituals."
          ++ "The priest, the last witness of the former life in the city, looks at you with an expression of concern."
          ++ "You can interact with him by interact Priest"
          ++ "The path to the forest is to the north.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (North, ("forest_cave", True)),
            (East, ("library", True))
          ]
    }

hillChurchSecondFloor :: Location
hillChurchSecondFloor =
  Location
    { locationName = "second_floor_of_hill_church",
      locationDescription =
        "You are now at the second floor of the church."
          ++ "You see some kind of weird numbers that seem out of order at the wall"
          ++ "4 6 1 2 6 7 3 4 1 5 6 2 7 3 5 7 3 2 5 3 6 4 3 6 7 2 "
          ++ "You notice also a sentence:\"That's a code to the truth of this mystery.\""
          ++ "To go back to first floor go east",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (East, ("hill_church", True))
          ]
    }

forestCave :: Location
forestCave =
  Location
    { locationName = "forest_cave",
      locationDescription =
        "You enter a dark cave hidden deep in the forest."
          ++ "In the center of the cave stands an altar with a strange symbol."
          ++ "The symbol looks familiar, it might be an ancient artifact sought by the archaeologists."
          ++ "Here you find evidence that the cult still exists and conducts its rituals here."
          ++ "You feel that this place may be key to solving the mystery of the archaeologists’ disappearance."
          ++ "You see far far to the north of this huge cave a weird doors that must lead to something."
          ++ "The path back leads south, returning to the church.",
      locationItems = [],
      locationInteractables = [],
      directions =
        Map.fromList
          [ (South, ("hill_church", True)),
            (North, ("ending_cave", True))
          ]
    }
