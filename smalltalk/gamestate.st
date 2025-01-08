Object subclass: GameState [
    | locationsMap currentLocation inventory locationItems closedLocations hillChurchEndingEscape gameOver flags |

    GameState class >> new [
        ^super new initialize
    ]

    initialize [
        locationsMap := Location createLocations.

        currentLocation := locationsMap at: 'train_station'.
        inventory := Array new. "Używamy Array"
        locationItems := Dictionary new.
        
        "Przypisanie OrderedCollection zamiast Array"
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
        flags := #().

        Transcript show: 'Initial location: ', currentLocation name; cr. 
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
        currentItems add: anItem.  "Poprawione dla OrderedCollection"
        locationItems at: currentLocation name put: currentItems.
    ]

    removeItemFromLocation: anItem [
        | currentItems |
        currentItems := locationItems at: currentLocation name ifAbsent: [OrderedCollection new].

        (currentItems includes: anItem) ifTrue: [
            currentItems := currentItems reject: [:item | item = anItem]. "Usuwanie z OrderedCollection"
            locationItems at: currentLocation name put: currentItems.
        ] ifFalse: [
            Transcript show: 'Item not found at the current location.'; cr.
        ].
    ]

    takeItem: anItem [
        | currentItems |
        Transcript show: 'Current location: ', currentLocation name; cr.
        
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
        inventory := inventory , (Array with: anItem).  "Poprawiona operacja łączenia"
    ]

    dropItem: anItem [
        | currentItems |
        currentItems := locationItems at: currentLocation name ifAbsent: [OrderedCollection new].
        currentItems add: anItem.
        locationItems at: currentLocation name put: currentItems.
        inventory := inventory reject: [:item | item = anItem].  "Poprawione z Array"
    ]

    checkItem: anItem [
        (inventory includes: anItem) ifTrue: [
            self displayItemDescription: anItem.
        ] ifFalse: [
            Transcript show: 'You do not have this item.'; cr.
        ].
    ]

    showInventory [
        Transcript show: 'Inventory: ', inventory printString; cr.
    ]

    displayItemDescription: anItem [
        | itemObject |
        itemObject := ItemObject findItem: anItem.
        itemObject ifNotNil: [
            Transcript show: itemObject description; cr.
        ].
    ]
]
