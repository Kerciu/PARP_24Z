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

    Location class >> parseDirection: aString [
        aString = 'n' ifTrue: [^'North'].
        aString = 'north' ifTrue: [^'North'].
        aString = 's' ifTrue: [^'South'].
        aString = 'south' ifTrue: [^'South'].
        aString = 'w' ifTrue: [^'West'].
        aString = 'west' ifTrue: [^'West'].
        aString = 'e' ifTrue: [^'East'].
        aString = 'east' ifTrue: [^'East'].
        ^nil.
    ]

    Location class >> createLocations [
        | trainStation parking car homelessBench riverTracks mainStreet oldTown hotelLobby hotelToilet hotelCorridor hotelRoom hotelBasement policeStation library archiveRoom hillChurch secondFloorOfHillChurch forestCave |

        trainStation := Location newWithName: 'train_station'
        description: 'You are at the train station, where your adventure started. ' ,
                     'The clock points at 3:15 am and never moves. ' ,
                     'The timetable is written in some out-of-this-world, unintelligible language. ' ,
                     'The only person present at the station is the caretaker. ' ,
                     'You can interact with her by interact caretaker. ' ,
                     'To the west, you can see the parking area adjacent to the station. ' ,
                     'To the east, there is a train which you can use to escape from city.'
        directions: (Dictionary new
                        at: 'West' put: 'parking';
                        at: 'East' put: 'escape_train').

        parking := Location newWithName: 'parking'
        description: 'You are in a deserted parking lot near the train station. The ground is littered' ,
                     'with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry.' ,
                     'To the north, you can see a homeless man sitting on a bench.' ,
                     'To the south is the main street,' ,
                     'and the train station is to the east.'
        directions: (Dictionary new
                        at: 'North' put: 'homeless_bench';
                        at: 'South' put: 'main_street';
                        at: 'East' put: 'train_station').

        car := Location newWithName: 'car'
        description: 'You sit inside the car, but it refuses to start. ' ,
                     'Type in exit_car in order to get out of the vehicle.'
        directions: (Dictionary new).

        homelessBench := Location newWithName: 'homeless_bench'
        description:  'You find yourself near a bench occupied by a homeless man, muttering under his breath.' ,
                     'You can interact with him by interact homeless' ,
                     'The parking area lies to the south.'
        directions: (Dictionary new at: 'South' put: 'parking').

        riverTracks := Location newWithName: 'river_tracks'
        description: 'You stand by the river tracks. The water flows sluggishly, casting eerie reflections' ,
                     'in the moonlight.'
        directions: (Dictionary new at: 'West' put: 'main_street').

        mainStreet := Location newWithName: 'main_street'
        description: 'You are on the main street, flanked by old, abandoned shops. The cracked windows' ,
                     'and faded signs give the area a ghostly feel.' ,
                     'To the west is the old town,' ,
                     'while the river tracks are to the east,' ,
                     'and the parking lot to the north.'
        directions: (Dictionary new
                        at: 'West' put: 'old_town';
                        at: 'East' put: 'river_tracks';
                        at: 'North' put: 'parking').

        oldTown := Location newWithName: 'old_town'
        description: 'You find yourself in the heart of Old Town,' ,
                     'a desolate square filled with abandoned shops and crumbling facades.' ,
                     'Dust and debris cover the cobblestone streets, and a faint echo of past lives lingers in the air.' ,
                     'In the north there is a police station,' ,
                     'in the east you can see the main street.' ,
                     'There is an old hotel to the south' ,
                     'and a library to the west.'
        directions: (Dictionary new
                        at: 'North' put: 'police_station';
                        at: 'South' put: 'hotel_lobby';
                        at: 'West' put: 'library';
                        at: 'East' put: 'main_street').

        hotelLobby := Location newWithName: 'hotel_lobby'
        description: 'You enter the lobby of the hotel.' ,
                     'There is glass scattered everywhere, and the old reception desk is covered in papers.' ,
                     'You can go north to return to Old Town.' ,
                     'There is a toilet to the east.' ,
                     'To the south, a dark hallway leads further into the hotel.' ,
                     'Looking to the west, you can see an elevator.'
        directions: (Dictionary new
                        at: 'North' put: 'old_town';
                        at: 'East' put: 'hotel_toilet';
                        at: 'South' put: 'hotel_corridor';
                        at: 'West' put: 'hotel_basement').

        hotelToilet := Location newWithName: 'hotel_toilet'
        description: 'You enter the toilet.' ,
                     'The room is dark and damp, and the smell of mold and decay fills the air.' ,
                     'The toilet is broken and the sink is covered in grime.' ,
                     'You can go west to return to the hotel lobby.'
        directions: (Dictionary new at: 'West' put: 'hotel_lobby').

        hotelCorridor := Location newWithName: 'hotel_corridor'
        description: 'You enter a dark hallway.' ,
                     'The ceiling has collapsed, thus most of the corridor is blocked off by rubble.' ,
                     'Only one room to the west remains accessible.' ,
                     'You can go north to return to the hotel lobby.'
        directions: (Dictionary new
                        at: 'North' put: 'hotel_lobby';
                        at: 'West' put: 'hotel_room').

        hotelRoom := Location newWithName: 'hotel_room'
        description: 'You enter the hotel room.' ,
                     'The room is dark and dusty, and the bed is covered in old sheets.' ,
                     'The closet is empty, and the desk is covered in papers.' ,
                     'You can go east to return to the corridor.'
        directions: (Dictionary new at: 'East' put: 'hotel_corridor').

        hotelBasement := Location newWithName: 'hotel_basement'
        description: 'The elevator can only go down.' ,
                     'It leads to the basement of the hotel.' ,
                     'The basement is dark and damp, with a faint, musty odor filling the air.' ,
                     'You can go east to return to the hotel lobby.'
        directions: (Dictionary new at: 'East' put: 'hotel_lobby').

        policeStation := Location newWithName: 'police_station'
        description: 'You enter the police station. The lights flicker, casting eerie shadows on the walls.' ,
                     'To the left, you see a dusty counter with an old safe behind it. ' ,
                     'The smell of cheap liquor and stale cigarettes fills the air.' ,
                     'A drunkard, with tangled hair and a worn-out jacket, stares at you with a glazed look.' ,
                     'He seems to be clutching an old bottle. Perhaps he knows something useful?' ,
                     'You can interact with him by interact drunkard' ,
                     'You can enter the code to the safe by open safe.' ,
                     'You can go south to return to the old town.'
        directions: (Dictionary new at: 'South' put: 'old_town').

        library := Location newWithName: 'library'
        description: 'You enter the library. The room is quiet, filled with towering shelves of old, dusty books.' ,
                     'There is the old town to the east.' ,
                     'To the south, you see a door leading to the archive room.' ,
                     'To the west, there is a church on the hill.'
        directions: (Dictionary new
                        at: 'East' put: 'old_town';
                        at: 'South' put: 'archive';
                        at: 'West' put: 'hill_church').

        archiveRoom := Location newWithName: 'archive'
        description: 'You enter the archive room. It is small and cramped, filled with stacks of old papers and documents.' ,
                     'Most of the documents are unreadable due to age.'
        directions: (Dictionary new at: 'North' put: 'library').

        hillChurch := Location newWithName: 'hill_church'
        description: 'You stand before an old, abandoned church on the hill.' ,
                     'The dark interior and the smell of incense remind you of ancient rituals.' ,
                     'The priest, the last witness of the former life in the city, looks at you with an expression of concern.' ,
                     'You can interact with him by interact priest' ,
                     'The path to the forest is to the north.' ,
                     'You can go west to the second floor of the church.' ,
                     'To the east, you see the library.'
        directions: (Dictionary new
                        at: 'North' put: 'forest_cave';
                        at: 'East' put: 'library';
                        at: 'West' put: 'second_floor_of_hill_church').

        secondFloorOfHillChurch := Location newWithName: 'second_floor_of_hill_church'
        description:  'You are now at the second floor of the church.' ,
                     'You see some kind of weird numbers that seem out of order at the wall' ,
                     '4 6 1 2 6 7 3 4 1 5 6 2 7 3 5 7 3 2 5 3 6 4 3 6 7 2 ' ,
                     'You notice also a sentence: That`s a code to the truth of this mystery.' ,
                     'To go back to first floor go east'
        directions: (Dictionary new at: 'East' put: 'hill_church').

        forestCave := Location newWithName: 'forest_cave'
        description: 'You enter a dark cave hidden deep in the forest.' ,
                     'In the center of the cave stands an altar with a strange symbol.' ,
                     'The symbol looks familiar, it might be an ancient artifact sought by the archaeologists.' ,
                     'Here you find evidence that the cult still exists and conducts its rituals here.' ,
                     'You feel that this place may be key to solving the mystery of the archaeologists disappearance.' ,
                     'You see far far to the north of this huge cave a weird doors that must lead to something.' ,
                     'The path back leads south, returning to the church.'
        directions: (Dictionary new at: 'South' put: 'hill_church'; at: 'North' put: 'weird_doors').

        strangeDoors := Location newWithName: 'weird_doors'
        description: 'The weird doors seem to have appeared by themselves from nowhere.' ,
                     'You open the doors and you have the feeling you are stepping back in time. The air smells of sulphur.'
        directions: (Dictionary new).
    ]
]