Object subclass: GameState [
    | locationsMap currentLocation inventory locationItems closedLocations hillChurchEndingEscape gameOver flags |

    GameState class >> new [
        ^super new initialize
    ]

    initialize [
        locationsMap := Location createLocations.

        currentLocation := locationsMap at: 'train_station'.
        inventory := Array new.
        locationItems := Dictionary new.
        
        locationItems at: 'hotel_basement' put: (OrderedCollection with: 'notes' with: 'amulet').
        locationItems at: 'car' put: (OrderedCollection with: 'key').
        locationItems at: 'hotel_room' put: (OrderedCollection with: 'diary' with: 'blueFuse').
        locationItems at: 'hotel_toilet' put: (OrderedCollection with: 'redFuse').
        locationItems at: 'library' put: (OrderedCollection with: 'newspaper').
        locationItems at: 'river_tracks' put: (OrderedCollection with: 'cigarettes').
        locationItems at: 'archive' put: (OrderedCollection with: 'greenFuse').

        closedLocations := #('hotel_room' 'hotel_basement' 'archive').
        hillChurchEndingEscape := false.
        gameOver := false.
        flags := Array new.
    ]

    currentLocation [
        ^currentLocation
    ]

    currentLocation: aLocation [
        currentLocation := aLocation
    ]

    inventory [
        ^inventory
    ]

    inventory: anArray [
        inventory := anArray
    ]    

    gameOver [
        ^gameOver
    ]

    updateGameOver [
        gameOver := true
    ]

    addItemToLocation: anItem [
        | currentItems |
        currentItems := locationItems at: currentLocation name ifAbsent: [OrderedCollection new].
        currentItems add: anItem.
        locationItems at: currentLocation name put: currentItems.
    ]

    removeItemFromLocation: anItem [
        | currentItems |
        currentItems := locationItems at: currentLocation name ifAbsent: [OrderedCollection new].

        (currentItems includes: anItem) ifTrue: [
            currentItems := currentItems reject: [:item | item = anItem]. 
            locationItems at: currentLocation name put: currentItems.
        ] ifFalse: [
            Transcript show: 'Item not found at the current location.'; cr.
        ].
    ]

    takeItem: anItem [
        | currentItems |
        currentItems := locationItems at: currentLocation name ifAbsent: [OrderedCollection new].
        
        (currentItems includes: anItem) ifTrue: [
            self removeItemFromLocation: anItem.
            self addToInventory: anItem.
            Transcript show: 'You have taken ', anItem printString; cr.
        ] ifFalse: [
            Transcript show: 'Item not found at the current location.'; cr.
        ].
    ]

    addToInventory: anItem [
        inventory := inventory , (Array with: anItem).
    ]

    dropItem: anItem [
        | currentItems |
        (inventory includes: anItem) ifTrue: [
        currentItems := locationItems at: currentLocation name ifAbsent: [OrderedCollection new].
        currentItems add: anItem.
        locationItems at: currentLocation name put: currentItems.
        inventory := inventory reject: [:item | item = anItem].
        ] ifFalse: [
            Transcript show: 'Item not found in your inventory.'; cr.
        ].
    ]

    checkItem: anItem [
        (inventory includes: anItem) ifTrue: [
            self displayItemDescription: anItem.
        ] ifFalse: [
            Transcript show: 'You do not have this item.'; cr.
        ].
    ]

    showInventory [
        Transcript show: 'Inventory:'; cr.
        inventory isEmpty
            ifTrue: [Transcript show: 'No items in inventory.'; cr.]
            ifFalse: [
                inventory do: [ :item | 
                    Transcript show: ' - ', item printString; cr. 
                ].
            ].
    ]

    displayItemDescription: anItem [
        | itemObject |
        itemObject := ItemObject findItem: anItem.
        itemObject ifNotNil: [
            Transcript show: itemObject description; cr.
        ].
    ]

    lookAround [
        | currentLocationName |
        currentLocationName := currentLocation name.

        Transcript show: 'Current Location: ', currentLocationName; cr.

        (currentLocationName = 'homeless_bench') ifTrue: [
            Transcript show: 'You find yourself near a bench occupied by a homeless man, muttering under his breath.'; cr.
            (flags includes: 'first_homeless_interaction') ifFalse: [
                Transcript show: 'He warns of the ''shadows that follow at night'' and clutches an old bottle with'; cr.
                Transcript show: 'You notice that he cannot breathe properly, he probably ran out of cigarettes.'; cr.
                flags := flags, #('first_homeless_interaction').
            ].
            (inventory includes: 'cigarettes') ifTrue: [
                Transcript show: 'You notice that you can help the homeless man with a cigarette.'; cr.
            ].
            Transcript show: 'You can interact with him by typing: interact homeless'; cr.
            Transcript show: 'The parking area lies to the south.'; cr.
        ] ifFalse: [

            (currentLocationName = 'hotel_basement') ifTrue: [
                Transcript show: 'The elevator can only go down.'; cr.
                Transcript show: 'It leads to the basement of the hotel.'; cr.
                Transcript show: 'The basement is dark and damp, with a faint, musty odor filling the air.'; cr.
                (isItemAt: 'amulet' inLocation: 'hotel_basement') ifTrue: [
                    Transcript show: 'Among the piles of old crates and broken furniture, something stands out -'; cr.
                    Transcript show: 'a strange amulet positioned in the center of the room.'; cr.
                ]. 
                (isItemAt: 'notes' inLocation: 'hotel_basement') ifTrue: [
                    Transcript show: 'You notice a set of old, crumbling notes scattered across a dusty table.'; cr.
                ].
                Transcript show: 'You can go east to return to the hotel lobby.'; cr.
            ] ifFalse: [

                (currentLocationName = 'archive') ifTrue: [
                    Transcript show: 'You enter the archive room. It is small and cramped, filled with stacks of old papers and documents.'; cr.
                    Transcript show: 'Most of the documents are unreadable due to age.'; cr.
                    (isItemAt: 'greenFuse' inLocation: 'archive') ifTrue: [
                        Transcript show: 'However you notice a bright green fuse lying on a nearby shelf.'; cr.
                    ].
                ] ifFalse: [

                    (currentLocationName = 'parking') ifTrue: [
                        Transcript show: 'You are in a deserted parking lot near the train station. The ground is littered'; cr.
                        Transcript show: 'with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry.'; cr.
                        Transcript show: 'To the north, you can see a homeless man sitting on a bench.'; cr.
                        Transcript show: 'To the south is the main street,'; cr.
                        Transcript show: 'and the train station is to the east.'; cr.
                        (inventory includes: 'car_keys') ifTrue: [
                            Transcript show: 'You have the car keys so you can try to open the abandoned car.'; cr.
                            Transcript show: 'Type in enter_car in order to get into the vehicle.'; cr.
                        ].
                    ] ifFalse: [

                        (currentLocationName = 'car') ifTrue: [
                            Transcript show: 'You sit inside the car, but it refuses to start.'; cr.
                            Transcript show: 'Type in exit_car in order to get out of the vehicle.'; cr.
                            (isItemAt: 'key' inLocation: 'car') ifTrue: [
                                Transcript show: 'You noticed an old key lying on the passenger seat.'; cr.
                            ].
                        ] ifFalse: [
                            Transcript show: currentLocation description; cr.
                        ]
                    ]
                ]
            ]
        ]

        items := locationItems at: currentLocation name ifAbsent: [#()].
        items isEmpty ifFalse: [
            items do: [ :item | 
                Transcript show: 'You see ', item printString, ' here.'; cr. 
            ].
        ].
    ]

    isItemAt: anItem inLocation: aLocation [
        | currentItems |
        currentItems := locationItems at: aLocation ifAbsent: [OrderedCollection new].
        ^currentItems includes: anItem.
    ]

    hasItem: anItem [
        ^inventory includes: anItem.
    ]
]
