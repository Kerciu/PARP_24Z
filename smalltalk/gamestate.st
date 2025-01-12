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

        closedLocations := (OrderedCollection with: 'hotel_room' with: 'hotel_basement' with: 'archive').
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

    flags [
        ^flags
    ]

    flags: aFlags [
        flags := aFlags
    ]

    inventory [
        ^inventory
    ]

    inventory: anArray [
        inventory := anArray
    ]

    locationItems [
        ^locationItems
    ] 

    locationItems: aLocationItems [
        locationItems := aLocationItems
    ]

    closedLocations [
        ^closedLocations
    ]

    closedLocations: aClosedLocations [
        closedLocations := aClosedLocations
    ]

    hillChurchEndingEscape [
        ^hillChurchEndingEscape
    ]

    hillChurchEndingEscape: aHillChurchEndingEscape [
        hillChurchEndingEscape := aHillChurchEndingEscape
    ]

    gameOver [
        ^gameOver
    ]

    updateGameOver [
        gameOver := true
    ]

    addItem: anItem [
        inventory := inventory , (Array with: anItem).
    ]

    removeItem: anItem [
        inventory := inventory reject: [:item | item = anItem].
    ]

    addFlag: aFlag [
        flags := flags , (Array with: aFlag).
    ]

    hasFlag: aFlag [
        ^flags includes: aFlag.
    ]

    notHasFlag: aFlag [
        ^(flags includes: aFlag) not.
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

    directionChange: direction [
        | currentLocationName nextLocation userInput |

        currentLocationName := currentLocation name.

        ((direction = 'east' or: [direction = 'e']) and: [currentLocationName = 'train_station'])
            ifTrue: [
                Transcript show: 'Are you sure you want to escape the city? (yes/no)'; cr.
                userInput := UIManager default request: 'Type your answer (yes/no):'.
                (userInput = 'yes')
                    ifTrue: [
                        Transcript show: (Endings escapeCityEndingText: (self hasItem: 'diary')); cr.
                        self updateGameOver.
                        ^self.
                    ]
                    ifFalse: [
                        Transcript show: 'You decide to stay in the city and look around.'; cr.
                        ^self.
                    ].
            ].

        ((direction = 'north' or: [direction = 'n']) and: [currentLocationName = 'forest_cave'])
            ifTrue: [
                (self hasItem: 'amulet')
                    ifTrue: [
                        Transcript show: 'The amulet glows faintly, and an ancient force gives way.'; cr.
                        (self hasItem: 'engraved_ring')
                            ifTrue: [
                                Transcript show: Endings forestCaveEndingWeakenedText; cr.
                            ]
                            ifFalse: [
                                Transcript show: Endings forestCaveEndingKilledText; cr.
                            ].
                        self updateGameOver.
                        ^self.
                    ]
                    ifFalse: [
                        Transcript show: 'A strange force blocks your path. Something is missing...'; cr.
                        ^self.
                    ].
            ].

        ((direction = 'north' or: [direction = 'n']) and: [currentLocationName = 'second_floor_of_hill_church'])
            ifTrue: [
                (self hasFlag: 'weird_box_opened')
                    ifTrue: [
                        Transcript show: Endings hillChurchEndingEscapeText; cr.
                        self updateGameOver.
                        ^self.
                    ]
                    ifFalse: [
                        Transcript show: 'You cannot go that way yet.'; cr.
                        ^self.
                    ].
            ].

        nextLocation := self currentLocation moveTo: direction locationsMap: locationsMap.
        nextLocation ifNotNil: [
            (self isLocationBlocked: nextLocation name) ifTrue: [
                self locationBlocked: nextLocation name.
            ] ifFalse: [
                self currentLocation: nextLocation.
                Transcript show: self currentLocation description; cr.
            ].
        ].
    ]

    isLocationBlocked: aLocation [
        ^closedLocations includes: aLocation.
    ]

    locationBlocked: nextLocationName [
        (closedLocations includes: nextLocationName) ifTrue: [
            (nextLocationName = 'hotel_room') ifTrue: [
                self openHotelRoom.
                ^self.
            ].

            (nextLocationName = 'hotel_basement') ifTrue: [
                self openHotelBasement.
                ^self.
            ].

            (nextLocationName = 'archive') ifTrue: [
                self openArchive.
                ^self.
            ].

            Transcript show: 'Error: Dont know how to open it'; cr.
        ] ifFalse: [
            currentLocation: (locationsMap at: nextLocationName).
        ]
    ]

    removeClosedLocation: aLocation [
        closedLocations := closedLocations reject: [:item | item = aLocation].
    ]

    openHotelRoom [
        | code |
        Transcript show: 'The door is locked.'; cr.
        Transcript show: 'However, you noticed a sign that says "Michael Turner".'; cr.
        Transcript show: 'There is also a keypad next to the door.'; cr.
        Transcript show: 'Please enter 4-digit code:'; cr.
        code := Dialog request: 'Enter the 4-digit code for the safe:'.

        (code = '1974') ifTrue: [
            Transcript show: 'Correct code!'; cr.
            self removeClosedLocation: 'hotel_room'.
            self directionChange: 'hotel_room'.
            
        ] ifFalse: [
            Transcript show: 'Wrong code!'; cr.
        ].
    ]

    openHotelBasement [
        ((self hasItem: 'redFuse') and: [(self hasItem: 'greenFuse') and: [(self hasItem: 'blueFuse')]]) ifTrue: [
            self removeItem: 'redFuse'.
            self removeItem: 'greenFuse'.
            self removeItem: 'blueFuse'.
            self removeClosedLocation: 'hotel_basement'.

            Transcript show: 'You placed the fuses in the fuse box and the elevator started working.'; cr.
            self directionChange: 'hotel_basement'.

        ] ifFalse: [
            Transcript show: 'The elevator is not working.'; cr.
            Transcript show: 'However, you noticed a fuse box next to the elevator with 3 fuses missing.'; cr.
            Transcript show: 'Maybe you can find them somewhere.'; cr.
        ].
    ]

    openArchive [
        (self hasItem: 'key') ifTrue: [
            self removeItem: 'key'.
            self removeClosedLocation: 'archive'.
            self directionChange: 'archive'.

            Transcript show: 'You have unlocked the archive.'; cr.
        ] ifFalse: [
            Transcript show: 'The archive is locked. You need a key to open it.'; cr.
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
                self addFlag 'first_homeless_interaction'.
            ].
            (hasItem: 'cigarettes') ifTrue: [
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

    giveItem: person to: item [
        (person = 'homeless' and: [(currentLocation name = 'homeless_bench') and: [(hasItem: 'cigarettes')]]) 
            ifTrue: [
                Transcript show: 'You give the pack of cigarettes to the homeless man. He takes them eagerly and thanks you.'; cr.
                Transcript show: 'He hands you a cold can of Harnas beer in return.'; cr.
                Transcript show: 'And a weird box saying "I found it a while ago, it is useless for me but maybe you can get it open"'; cr.
                self addFlag: 'first_homeless_interaction'.
                self addItem: 'weird_box'.
                self addItem: 'harnas'.
                self removeItem: 'cigarettes'.
            ]
            ifFalse: [
                (person = 'homeless' and: [(currentLocation name = 'homeless_bench') and: [(hasItem: 'cigarettes')]]) 
                    ifTrue: [
                        Transcript show: 'The homeless man rejects your offer to give him Harnas beer. He already has plenty of those.'; cr.
                        ^state.
                    ].
                (person = 'caretaker' and: [(currentLocation name = 'train_station') and: [(hasItem: 'harnas')]]) 
                    ifTrue: [
                        Transcript show: 'You offer the Harnas beer to the caretaker. She takes it gratefully and takes a swig.'; cr.
                        Transcript show: '''Ahh, that takes me back,'' she sighs and tells you more about her story with the homeless man.'; cr.
                        Transcript show: 'Grateful, she hands you the car keys.'; cr.
                        self addItem: 'car_keys'.
                        self removeItem: 'harnas'.
                    ].
                (person = 'drunkard' and: [(currentLocation name = 'police_station') and: [(hasItem: 'kuflowe_mocne')]]) 
                    ifTrue: [
                        Transcript show: 'The drunkard takes the Kuflowe Mocne with a greedy smile.'; cr.
                        Transcript show: '''Alright, alright... here, take this.'''; cr.
                        Transcript show: 'He hands you a crumpled leaf with the safe code scrawled on it.'; cr.
                        self addFlag: 'second_drunkard_interaction'.
                        self addItem: 'leaf_with_code'.
                        self removeItem: 'kuflowe_mocne'.
                    ].
                Transcript show: 'You cannot give this item to that person.'; cr.
            ].
        ^state.
    ]

    interactWith: person [
        (person = 'homeless' and: [(currentLocation name = 'homeless_bench')]) 
            ifTrue: [
                (hasItem: 'cigarettes') 
                    ifTrue: [
                        Transcript show: 'The homeless man seems to be more interested as he coughs intensively.'; cr.
                    ] 
                    ifFalse: [
                        (hasFlag: 'first_drunkard_interaction' and: [(notHasFlag: 'second_homeless_interaction')]) 
                            ifTrue: [
                                Transcript show: 'The homeless man looks at you knowingly.'; cr.
                                Transcript show: 'Oh, you need a drink for ol Bill? Here, take this Kuflowe Mocne. But dont tell him I gave it for free!'; cr.
                                self addItem: 'kuflowe_mocne'.
                                self addFlag: 'second_homeless_interaction'.
                            ]
                            ifFalse: [
                                (hasItem: 'amulet') 
                                    ifTrue: [
                                        Transcript show: 'The homeless man looks at you with wide eyes. ''You hold that cursed thing,'' he says, his voice shaking. ''I feel something watching... beware.'''; cr.
                                    ]
                                    ifFalse: [
                                        Transcript show: 'The homeless man seems to be uninterested for now.'; cr.
                                    ].
                            ].
            ] 
            ifFalse: [
                Transcript show: 'There is no homeless man here'; cr.
            ].

        (person = 'caretaker' and: [(currentLocation name = 'train_station')]) 
            ifTrue: [
                (hasItem: 'amulet') 
                    ifTrue: [
                        Transcript show: 'The caretaker looks at you uneasily as you hold the strange amulet. "What is that you have there?" she asks, her eyes narrowing. ''You should be careful where you go with that.'''; cr.
                    ] 
                    ifFalse: [
                        (hasItem: 'harnas') 
                            ifTrue: [
                                Transcript show: 'The caretaker seems more enticed to talk as she sees a can of cold Harnas.'; cr.
                            ] 
                            ifFalse: [
                                Transcript show: 'The caretaker seems more distant and uninterested for now.'; cr.
                            ].
                    ].
            ]
            ifFalse: [
                Transcript show: 'There is no caretaker here'; cr.
            ].

        (person = 'drunkard' and: [(currentLocation name = 'police_station')]) 
            ifTrue: [
                (self hasFlag: 'second_drunkard_interaction') 
                    ifTrue: [
                        Transcript show: 'I''ve already helped you, now let me enjoy my beer.'; cr.
                    ]
                    ifFalse: [
                        Transcript show: 'The drunkard looks at you with a smirk.'; cr.
                        Transcript show: '''Lookin'' for the safe code, eh? I might remember it... But I''m real thirsty.'''; cr.
                        Transcript show: '''Maybe if you bring me something to drink, I''ll let you in on the secret.'''; cr.
                        self addFlag: 'first_drunkard_interaction'.
                    ].
            ] 
            ifFalse: [
                Transcript show: 'There is no drunkard here'; cr.
            ].

        (person = 'priest' and: [(currentLocation name = 'hill_church')]) 
            ifTrue: [
                (self hasItem: 'amulet') 
                    ifTrue: [
                        Transcript show: 'The priest notices the amulet in your hand and warns you: That is the symbol of their cult; do not approach them with it.'; cr.
                        Transcript show: 'He continues: I can''t tell you more, just be careful'; cr.
                        Transcript show: 'He points at the old staircase, at the west side of the church, that leads to second floor of the church'; cr.
                    ]
                    ifFalse: [
                        '''Our Father, Who art in heaven, Hallowed be Thy name. Thy kingdom come, Thy will be done, On earth as it is in heaven...'''; cr.
                    ].
            ] 
            ifFalse: [
                Transcript show: 'There is no priest here'; cr.
            ].
        ]
    ]

    enterCar [
        | currentLocationName |
        currentLocationName := currentLocation name.

        (currentLocationName = 'parking') ifTrue: [
            (self hasFlag: 'car_opened') ifTrue: [
                currentLocation: (locationsMap at: 'car').
            ] ifFalse: [
                (self hasItem: 'car_keys') ifTrue: [
                    Transcript show: 'You unlock the car with keys.'; cr.
                    self removeItem: 'car_keys'.
                    self addFlag: 'car_opened'.
                    currentLocation: (locationsMap at: 'car').
                ] ifFalse: [
                    Transcript show: 'You need keys to enter the car.'; cr.
                ].
            ].

        ] ifFalse: [
            (currentLocationName = 'car') ifTrue: [
                Transcript show: 'You are already in the car.'; cr.
            ] ifFalse: [
                Transcript show: 'There is no car here.'; cr.
            ].
        ].
    ]

    exitCar [
        | currentLocationName |
        currentLocationName := currentLocation name.

        (currentLocationName = 'car') ifTrue: [
            currentLocation: (locationsMap at: 'parking').
            Transcript show: 'You exit the car and return to the parking lot.'; cr.
        ] ifFalse: [
            Transcript show: 'You are not in the car.'; cr.
        ].
    ]

    openObject: thing [
        | currentLocationName code |
        currentLocationName := currentLocation name.

        (thing = 'weird_box') ifTrue: [
            (self hasItem: 'weird_box') ifTrue: [
                (currentLocationName = 'second_floor_of_hill_church') ifTrue: [
                    Transcript show: 'You open the box with the code and find weird notes inside.'; cr.
                    Transcript show: 'One of those says: U CANNOT DEFEAT THEM FLEE FROM THE CITY AS FAST AS YOU CAN, THEY ARE CLOSING UP ON ME. THIS IS MY FAREWELL, GOODBYE THE ONE THAT READ THOOSE.'; cr.
                    Transcript show: 'You are now able to decide, whether to continue your journey or run away from the city.'; cr.
                    Transcript show: 'Decide fast or maybe you will meet the same fate as a writer of thoose notes.'; cr.
                    Transcript show: 'To flee from the city to train station you have to go north for a secret passage in the church.'; cr.
                    Transcript show: 'To return to the first floor of the church go east.'; cr.
                    self addFlag: 'weird_box_opened'.
                ] ifFalse: [
                    Transcript show: 'You have to know a 20-digit code to open this box.'; cr.
                    Transcript show: 'Maybe you can find it somewhere.'; cr.
                ].
            ] ifFalse: [
                Transcript show: 'You don''t have weird_box in your inventory.'; cr.
            ].
        ] ifFalse: [
            (thing = 'safe') ifTrue: [
                (currentLocationName = 'police_station') ifTrue: [
                    (self hasFlag: 'safe_opened') ifTrue: [
                        Transcript show: 'The safe is already opened.'; cr.
                    ] ifFalse: [
                        code := Dialog request: 'Enter the 4-digit code for the safe:'.
                        (code = '2137') ifTrue: [
                            Transcript show: 'The safe opens, revealing an engraved ring inside.'; cr.
                            self addItem: 'engraved_ring'.
                            self addFlag: 'safe_opened'.
                        ] ifFalse: [
                            Transcript show: 'Incorrect code. The safe remains locked.'; cr.
                        ].
                    ].
                ] ifFalse: [
                    Transcript show: 'There is no safe here.'; cr.
                ].
            ] ifFalse: [
                Transcript show: 'You cannot open that.'; cr.
            ].
        ].
    ]
]
