Object subclass: MovementManager [
    | currentLocation locationsMap | "Instance variables"

    MovementManager class >> new [
        ^self new initialize
    ]

    initialize [
        "Initialize the MovementManager with a default location and locations map"
        currentLocation := 'train_station'.
        locationsMap := Location createLocations.
    ]

    currentLocation [
        ^currentLocation.
    ]

    moveTo: direction [
        | parsedDirection currentLocationObject nextLocationName nextLocationObject |
        parsedDirection := Location parseDirection: direction.

        "Get the current location object"
        currentLocationObject := locationsMap at: currentLocation ifAbsent: [nil].

        "Check if the current location has valid directions"
        currentLocationObject ifNotNil: [
            nextLocationName := currentLocationObject directions at: parsedDirection ifAbsent: [nil].
            nextLocationName ifNotNil: [
                nextLocationObject := locationsMap at: nextLocationName ifAbsent: [nil].
                nextLocationObject ifNotNil: [
                    currentLocation := nextLocationName.
                    self printCurrentLocation.
                ] ifNil: [
                    Transcript show: 'The location "', nextLocationName, '" does not exist.'; cr.
                ].
            ] ifNil: [
                Transcript show: 'You cannot go ', direction, ' from here.'; cr.
            ].
        ] ifNil: [
            Transcript show: 'Current location is invalid.'; cr.
        ].
    ]

    printCurrentLocation [
        | locationObject |
        locationObject := locationsMap at: currentLocation ifAbsent: [nil].
        locationObject ifNotNil: [
            Transcript show: locationObject name; cr.
            Transcript show: locationObject description; cr.
        ] ifNil: [
            Transcript show: 'Unknown location.'; cr.
        ].
    ]
]
