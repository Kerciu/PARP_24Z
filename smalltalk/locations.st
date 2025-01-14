Object subclass: Location [

    | name description directions |

    Location class >> newWithName: aName description: aDescription directions: aDirections [
        ^self new
            name: aName;
            description: aDescription;
            directions: aDirections;
            yourself.
    ]

    Location >> name [
        ^name.
    ]

    Location >> name: aName [
        name := aName.
    ]

    Location >> description [
        ^description.
    ]

    Location >> description: aDescription [
        description := aDescription.
    ]

    Location >> directions [
        ^directions.
    ]

    Location >> directions: aDirections [
        directions := aDirections.
    ]

    Location >> moveTo: aDirection locationsMap: locationsMap [
        | nextLocationName nextLocation |

        nextLocationName := self directions at: aDirection ifAbsent: [nil].
        nextLocationName ifNotNil: [
            nextLocation := locationsMap at: nextLocationName ifAbsent: [nil].
            nextLocation ifNotNil: [^nextLocation].
            Transcript show: 'The location "', nextLocationName, '" does not exist.'; cr.
            ^nil.
        ].

        Transcript show: 'You cannot go there.'; cr.
        ^nil.
    ]

    Location class >> createLocations [
        | newline locationsMap trainStation parking car homelessBench riverTracks mainStreet oldTown hotelLobby hotelToilet hotelCorridor hotelRoom hotelBasement policeStation library archiveRoom hillChurch secondFloorOfHillChurch forestCave |

        locationsMap := Dictionary new.
        newline := (Character value: 10).

        trainStation := Location newWithName: 'train_station'
            description: 'You are at the train station, where your adventure started. ' , newline asString,
                        'The clock points at 3:15 am and never moves. ' , newline asString,
                        'The timetable is written in some out-of-this-world, unintelligible language. ' , newline asString,
                        'The only person present at the station is the caretaker. ' , newline asString,
                        'You can interact with her by interact caretaker. ' , newline asString,
                        'To the west, you can see the parking area adjacent to the station. ' , newline asString,
                        'To the east, there is a train which you can use to escape from city.'
            directions: (Dictionary new).
        trainStation directions at: 'w' put: 'parking'.

        parking := Location newWithName: 'parking'
            description: 'You are in a deserted parking lot near the train station. The ground is littered' , newline asString,
                        'with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry.' , newline asString,
                        'To the north, you can see a homeless man sitting on a bench.' , newline asString,
                        'To the south is the main street,' , newline asString,
                        'and the train station is to the east.'
            directions: (Dictionary new).
        parking directions at: 'n' put: 'homeless_bench'.
        parking directions at: 'e' put: 'train_station'.
        parking directions at: 's' put: 'main_street'.

        car := Location newWithName: 'car'
            description: 'You sit inside the car, but it refuses to start. ' , newline asString,
                        'Type in exit_car in order to get out of the vehicle.'
            directions: (Dictionary new).

        homelessBench := Location newWithName: 'homeless_bench'
            description: 'You find yourself near a bench occupied by a homeless man, muttering under his breath.' , newline asString,
                        'You can interact with him by interact homeless' , newline asString,
                        'The parking area lies to the south.'
            directions: (Dictionary new).
        homelessBench directions at: 's' put: 'parking'.

        riverTracks := Location newWithName: 'river_tracks'
            description: 'You stand by the river tracks. The water flows sluggishly, casting eerie reflections' , newline asString,
                        'in the moonlight.'
            directions: (Dictionary new).
        riverTracks directions at: 'w' put: 'main_street'.

        mainStreet := Location newWithName: 'main_street'
            description: 'You are on the main street, flanked by old, abandoned shops. The cracked windows' , newline asString,
                        'and faded signs give the area a ghostly feel.' , newline asString,
                        'To the west is the old town,' , newline asString,
                        'while the river tracks are to the east,' , newline asString,
                        'and the parking lot to the north.'
            directions: (Dictionary new).
        mainStreet directions at: 'w' put: 'old_town'.
        mainStreet directions at: 'e' put: 'river_tracks'.
        mainStreet directions at: 'n' put: 'parking'.

        oldTown := Location newWithName: 'old_town'
            description: 'You find yourself in the heart of Old Town,' , newline asString,
                        'a desolate square filled with abandoned shops and crumbling facades.' , newline asString,
                        'Dust and debris cover the cobblestone streets, and a faint echo of past lives lingers in the air.' , newline asString,
                        'In the north there is a police station,' , newline asString,
                        'in the east you can see the main street.' , newline asString,
                        'There is an old hotel to the south' , newline asString,
                        'and a library to the west.'
            directions: (Dictionary new).
        oldTown directions at: 'n' put: 'police_station'.
        oldTown directions at: 's' put: 'hotel_lobby'.
        oldTown directions at: 'w' put: 'library'.
        oldTown directions at: 'e' put: 'main_street'.

        hotelLobby := Location newWithName: 'hotel_lobby'
            description: 'You enter the lobby of the hotel.' , newline asString,
                        'There is glass scattered everywhere, and the old reception desk is covered in papers.' , newline asString,
                        'You can go north to return to Old Town.' , newline asString,
                        'There is a toilet to the east.' , newline asString,
                        'To the south, a dark hallway leads further into the hotel.' , newline asString,
                        'Looking to the west, you can see an elevator.'
            directions: (Dictionary new).
        hotelLobby directions at: 'n' put: 'old_town'.
        hotelLobby directions at: 'e' put: 'hotel_toilet'.
        hotelLobby directions at: 's' put: 'hotel_corridor'.
        hotelLobby directions at: 'w' put: 'hotel_basement'.

        hotelToilet := Location newWithName: 'hotel_toilet'
            description: 'You enter the toilet.' , newline asString,
                        'The room is dark and damp, and the smell of mold and decay fills the air.' , newline asString,
                        'The toilet is broken and the sink is covered in grime.' , newline asString,
                        'You can go west to return to the hotel lobby.'
            directions: (Dictionary new).
        hotelToilet directions at: 'w' put: 'hotel_lobby'.

        hotelCorridor := Location newWithName: 'hotel_corridor'
            description: 'You enter a dark hallway.' , newline asString,
                        'The ceiling has collapsed, thus most of the corridor is blocked off by rubble.' , newline asString,
                        'Only one room to the west remains accessible.' , newline asString,
                        'You can go north to return to the hotel lobby.'
            directions: (Dictionary new).
        hotelCorridor directions at: 'n' put: 'hotel_lobby'.
        hotelCorridor directions at: 'w' put: 'hotel_room'.

        hotelRoom := Location newWithName: 'hotel_room'
            description: 'You enter the hotel room.' , newline asString,
                        'The room is dark and dusty, and the bed is covered in old sheets.' , newline asString,
                        'The closet is empty, and the desk is covered in papers.' , newline asString,
                        'You can go east to return to the corridor.'
            directions: (Dictionary new).
        hotelRoom directions at: 'e' put: 'hotel_corridor'.

        hotelBasement := Location newWithName: 'hotel_basement'
            description: 'The elevator can only go down.' , newline asString,
                        'It leads to the basement of the hotel.' , newline asString,
                        'The basement is dark and damp, with a faint, musty odor filling the air.' , newline asString,
                        'You can go east to return to the hotel lobby.'
            directions: (Dictionary new).
        hotelBasement directions at: 'e' put: 'hotel_lobby'.

        policeStation := Location newWithName: 'police_station'
            description: 'You enter the police station. The lights flicker, casting eerie shadows on the walls.' , newline asString,
                        'To the left, you see a dusty counter with an old safe behind it. ' , newline asString,
                        'The smell of cheap liquor and stale cigarettes fills the air.' , newline asString,
                        'A drunkard, with tangled hair and a worn-out jacket, stares at you with a glazed look.' , newline asString,
                        'He seems to be clutching an old bottle. Perhaps he knows something useful?' , newline asString,
                        'You can interact with him by interact drunkard' , newline asString,
                        'You can enter the code to the safe by open safe.' , newline asString,
                        'You can go south to return to the old town.'
            directions: (Dictionary new).
        policeStation directions at: 's' put: 'old_town'.

        library := Location newWithName: 'library'
            description: 'You enter the library. The room is quiet, filled with towering shelves of old, dusty books.' , newline asString,
                        'There is the old town to the east.' , newline asString,
                        'To the south, you see a door leading to the archive room.' , newline asString,
                        'To the west, there is a church on the hill.'
            directions: (Dictionary new).
        library directions at: 'e' put: 'old_town'.
        library directions at: 's' put: 'archive'.
        library directions at: 'w' put: 'hill_church'.

        archiveRoom := Location newWithName: 'archive'
            description: 'You enter the archive room. It is small and cramped, filled with stacks of old papers and documents.' , newline asString,
                        'Most of the documents are unreadable due to age.'
            directions: (Dictionary new).
        archiveRoom directions at: 'n' put: 'library'.

        hillChurch := Location newWithName: 'hill_church'
            description: 'You stand before an old, abandoned church on the hill.' , newline asString,
                        'The dark interior and the smell of incense remind you of ancient rituals.' , newline asString,
                        'The priest, the last witness of the former life in the city, looks at you with an expression of concern.' , newline asString,
                        'You can interact with him by interact priest' , newline asString,
                        'The path to the forest is to the north.' , newline asString,
                        'You can go west to the second floor of the church.' , newline asString,
                        'To the east, you see the library.'
            directions: (Dictionary new).
        hillChurch directions at: 'n' put: 'forest_cave'.
        hillChurch directions at: 'e' put: 'library'.
        hillChurch directions at: 'w' put: 'second_floor_of_hill_church'.

        secondFloorOfHillChurch := Location newWithName: 'second_floor_of_hill_church'
            description: 'You are now at the second floor of the church.' , newline asString,
                        'You see some kind of weird numbers that seem out of order at the wall' , newline asString,
                        '4 6 1 2 6 7 3 4 1 5 6 2 7 3 5 7 3 2 5 3 6 4 3 6 7 2 ' , newline asString,
                        'You notice also a sentence: That`s a code to the truth of this mystery.' , newline asString,
                        'To go back to first floor go east'
            directions: (Dictionary new).
        secondFloorOfHillChurch directions at: 'e' put: 'hill_church'.

        forestCave := Location newWithName: 'forest_cave'
            description: 'You enter a dark cave hidden deep in the forest.' , newline asString,
                        'In the center of the cave stands an altar with a strange symbol.' , newline asString,
                        'The symbol looks familiar, it might be an ancient artifact sought by the archaeologists.' , newline asString,
                        'Here you find evidence that the cult still exists and conducts its rituals here.' , newline asString,
                        'You feel that this place may be key to solving the mystery of the archaeologists disappearance.' , newline asString,
                        'You see far far to the north of this huge cave a weird doors that must lead to something.' , newline asString,
                        'The path back leads south, returning to the church.'
            directions: (Dictionary new).
        forestCave directions at: 's' put: 'hill_church'.

        locationsMap at: 'train_station' put: trainStation.
        locationsMap at: 'parking' put: parking.
        locationsMap at: 'car' put: car.
        locationsMap at: 'homeless_bench' put: homelessBench.
        locationsMap at: 'river_tracks' put: riverTracks.
        locationsMap at: 'main_street' put: mainStreet.
        locationsMap at: 'old_town' put: oldTown.
        locationsMap at: 'hotel_lobby' put: hotelLobby.
        locationsMap at: 'hotel_toilet' put: hotelToilet.
        locationsMap at: 'hotel_corridor' put: hotelCorridor.
        locationsMap at: 'hotel_room' put: hotelRoom.
        locationsMap at: 'hotel_basement' put: hotelBasement.
        locationsMap at: 'police_station' put: policeStation.
        locationsMap at: 'library' put: library.
        locationsMap at: 'archive' put: archiveRoom.
        locationsMap at: 'hill_church' put: hillChurch.
        locationsMap at: 'second_floor_of_hill_church' put: secondFloorOfHillChurch.
        locationsMap at: 'forest_cave' put: forestCave.

        ^locationsMap
    ]
]
