Object subclass: GameState [
    | currentLocation inventory locationItems closedLocations hillChurchEndingEscape gameOver flags |

    GameState class >> new [
        ^super new initialize
    ]

    initialize [
        currentLocation := 'trainStation'.
        inventory := #().
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
    ]

    gameOver [
        ^gameOver
    ]

    updateGameOver [
        gameOver := true
    ]
]
