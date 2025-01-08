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
        locationItems at: 'hotel_basement' put: #('notes' 'amulet').
        locationItems at: 'car' put: #('key').
        locationItems at: 'hotel_room' put: #('diary' 'blueFuse').
        locationItems at: 'hotel_toilet' put: #('redFuse').
        locationItems at: 'library' put: #('newspaper').
        locationItems at: 'river_tracks' put: #('cigarettes').
        locationItems at: 'archive' put: #('greenFuse').

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
        currentItems := locationItems at: currentLocation name ifAbsent: [#()].
        currentItems := currentItems add: anItem.  "Dodajemy element do Array"
        locationItems at: currentLocation name put: currentItems.
    ]

    removeItemFromLocation: anItem [
        | currentItems |
        currentItems := locationItems at: currentLocation name ifAbsent: [#()].

        (currentItems includes: anItem) ifTrue: [
            currentItems := currentItems reject: [:item | item = anItem]. "Array nie pozwala na usuwanie wprost"
            locationItems at: currentLocation name put: currentItems.
        ] ifFalse: [
            Transcript show: 'Item not found at the current location.'; cr.
        ].
    ]

    takeItem: anItem [
        | currentItems |
        Transcript show: 'Current location: ', currentLocation name; cr.
        
        currentItems := locationItems at: currentLocation name ifAbsent: [#()].
        
        (currentItems includes: anItem) ifTrue: [
            self removeItemFromLocation: anItem.
            self addToInventory: anItem.
            Transcript show: 'You have taken ', anItem printString; cr.
        ] ifFalse: [
            Transcript show: 'Item not found at the current location.'; cr.
        ].
    ]

    addToInventory: anItem [
        inventory := inventory , (Array with: anItem).  "Array ma operator , do łączenia tablic"
    ]

    dropItem: anItem [
        | currentItems |
        currentItems := locationItems at: currentLocation name ifAbsent: [#()].
        currentItems := currentItems add: anItem.
        locationItems at: currentLocation name put: currentItems.
        inventory := inventory reject: [:item | item = anItem].  "Array nie ma metody remove:, ale możemy użyć reject"
    ]

    checkItem: anItem [
        self displayItemDescription: anItem.
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
