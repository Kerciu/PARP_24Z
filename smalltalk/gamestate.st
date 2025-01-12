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
        flags := OrderedCollection new.
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

    isItemAt: anItem inLocation: aLocation [
        | currentItems |
        currentItems := locationItems at: aLocation ifAbsent: [OrderedCollection new].
        ^currentItems includes: anItem.
    ]

    hasItem: anItem [
        ^inventory includes: anItem.
    ]

    itemsAtLocation: aLocation [
        ^locationItems at: aLocation ifAbsent: [OrderedCollection new].
    ]

    lookAround [
        | currentLocationName items |
        currentLocationName := self currentLocation name.

        (currentLocationName = 'homeless_bench') ifTrue: [
            Transcript show: 'You find yourself near a bench occupied by a homeless man, muttering under his breath.'; cr.
            (self hasFlag: 'first_homeless_interaction') ifFalse: [
                Transcript show: 'He warns of the ''shadows that follow at night'' and clutches an old bottle with'; cr.
                Transcript show: 'You notice that he cannot breathe properly, he probably ran out of cigarettes.'; cr.
                self addFlag: 'first_homeless_interaction'.
            ].
            (self hasItem: 'cigarettes') ifTrue: [
                Transcript show: 'You notice that you can help the homeless man with a cigarette.'; cr.
            ].
            Transcript show: 'You can interact with him by typing: interact homeless'; cr.
            Transcript show: 'The parking area lies to the south.'; cr.
        ] ifFalse: [
            (currentLocationName = 'hotel_basement') ifTrue: [
                Transcript show: 'The elevator can only go down.'; cr.
                Transcript show: 'It leads to the basement of the hotel.'; cr.
                Transcript show: 'The basement is dark and damp, with a faint, musty odor filling the air.'; cr.
                (self isItemAt: 'amulet' inLocation: 'hotel_basement') ifTrue: [
                    Transcript show: 'Among the piles of old crates and broken furniture, something stands out -'; cr.
                    Transcript show: 'a strange amulet positioned in the center of the room.'; cr.
                ].
                (self isItemAt: 'notes' inLocation: 'hotel_basement') ifTrue: [
                    Transcript show: 'You notice a set of old, crumbling notes scattered across a dusty table.'; cr.
                ].
                Transcript show: 'You can go east to return to the hotel lobby.'; cr.
            ] ifFalse: [

                (currentLocationName = 'archive') ifTrue: [
                    Transcript show: 'You enter the archive room. It is small and cramped, filled with stacks of old papers and documents.'; cr.
                    Transcript show: 'Most of the documents are unreadable due to age.'; cr.
                    (self isItemAt: 'greenFuse' inLocation: 'archive') ifTrue: [
                        Transcript show: 'However you notice a bright green fuse lying on a nearby shelf.'; cr.
                    ].
                ] ifFalse: [

                    (currentLocationName = 'parking') ifTrue: [
                        Transcript show: 'You are in a deserted parking lot near the train station. The ground is littered'; cr.
                        Transcript show: 'with old tickets and rusted cans. One car looks abandoned, as if the driver left in a hurry.'; cr.
                        Transcript show: 'To the north, you can see a homeless man sitting on a bench.'; cr.
                        Transcript show: 'To the south is the main street,'; cr.
                        Transcript show: 'and the train station is to the east.'; cr.
                        (self hasItem: 'car_keys') ifTrue: [
                            Transcript show: 'You have the car keys so you can try to open the abandoned car.'; cr.
                            Transcript show: 'Type in enter_car in order to get into the vehicle.'; cr.
                        ].
                    ] ifFalse: [

                        (currentLocationName = 'car') ifTrue: [
                            Transcript show: 'You sit inside the car, but it refuses to start.'; cr.
                            Transcript show: 'Type in exit_car in order to get out of the vehicle.'; cr.
                            (self isItemAt: 'key' inLocation: 'car') ifTrue: [
                                Transcript show: 'You noticed an old key lying on the passenger seat.'; cr.
                            ].
                        ] ifFalse: [
                            Transcript show: currentLocation description; cr.
                        ]
                    ]
                ]
            ]
        ].

        items := self itemsAtLocation: currentLocationName.
        items ifNotNil: [
            items do: [ :item | 
                Transcript show: 'You see ', item, ' here.'; cr. 
            ].
        ].
    ]

    giveItem: person to: item [
        | currentLocationName |
        
        currentLocationName := self currentLocation name.

        (person = 'homeless' and: [item = 'cigarettes'])
            ifTrue: [
                (currentLocationName = 'homeless_bench' and: [self hasItem: 'cigarettes'])
                    ifTrue: [
                        Transcript show: 'You give the pack of cigarettes to the homeless man. He takes them eagerly and thanks you.'; cr.
                        Transcript show: 'He hands you a cold can of Harnas beer in return.'; cr.
                        Transcript show: 'And a weird box saying "I found it a while ago, it is useless for me but maybe you can get it open"'; cr.
                        self addFlag: 'first_homeless_interaction'.
                        self addItem: 'weird_box'.
                        self addItem: 'harnas'.
                        self removeItem: 'cigarettes'.
                        ^self.
                    ].
            ].
        
        (person = 'homeless' and: [item = 'harnas']) 
            ifTrue: [
                (currentLocationName = 'homeless_bench' and: [self hasItem: 'harnas'])
                    ifTrue: [
                        Transcript show: 'The homeless man rejects your offer to give him Harnas beer. He already has plenty of those.'; cr.
                        ^self.
                    ].
            ].
        
        (person = 'caretaker' and: [item = 'harnas'])
            ifTrue: [
                (currentLocationName = 'train_station' and: [self hasItem: 'harnas'])
                    ifTrue: [
                        Transcript show: 'You offer the Harnas beer to the caretaker. She takes it gratefully and takes a swig.'; cr.
                        Transcript show: '''Ahh, that takes me back,'' she sighs and tells you more about her story with the homeless man.'; cr.
                        Transcript show: 'Grateful, she hands you the car keys.'; cr.
                        self addItem: 'car_keys'.
                        self removeItem: 'harnas'.
                        ^self.
                    ].
            ].

        (person = 'drunkard' and: [(currentLocation name = 'police_station') and: [self hasItem: 'kuflowe_mocne']]) 
            ifTrue: [
                Transcript show: 'The drunkard takes the Kuflowe Mocne with a greedy smile.'; cr.
                Transcript show: '''Alright, alright... here, take this.'''; cr.
                Transcript show: 'He hands you a crumpled leaf with the safe code scrawled on it.'; cr.
                self addFlag: 'second_drunkard_interaction'.
                self addItem: 'leaf_with_code'.
                self removeItem: 'kuflowe_mocne'.
                ^self.
            ].

        Transcript show: 'You cannot give this item to that person.'; cr.
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
]
