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
    | locationsMap trainStation parking car homelessBench riverTracks mainStreet oldTown hotelLobby hotelToilet hotelCorridor hotelRoom hotelBasement policeStation library archiveRoom hillChurch secondFloorOfHillChurch forestCave |

    locationsMap := Dictionary new.

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

    "Add all locations to the locationsMap"
    locationsMap at: 'train_station' put: trainStation.
    locationsMap at: 'parking' put: parking.
    locationsMap at: 'car' put: car.
    locationsMap at: 'homeless_bench' put: homelessBench.
    locationsMap at: 'river_tracks' put: riverTracks.
    locationsMap at: 'main_street' put: mainStreet.

    "Continue adding all other locations similarly"
    "Ensure that all locations are added to the map correctly"

    ^locationsMap "Return the dictionary containing all locations"
]
]