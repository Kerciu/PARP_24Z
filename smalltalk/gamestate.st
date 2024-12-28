Object subclass: GameState [
    | currentLocation inventory locationItems closedLocations hillChurchEndingEscape gameOver flags |

    GameState class >> new [
        ^super new initialize
    ]

    initialize [
        currentLocation := 'trainStation'.
        inventory := #().
        locationItems := Dictionary newFromPairs: {
            'hotel_basement' -> #('notes' 'amulet').
            'car' -> #('key').
            'hotel_room' -> #('diary' 'blueFuse').
            'hotel_toilet' -> #('redFuse').
            'library' -> #('newspaper').
            'river_tracks' -> #('cigarettes').
            'archive' -> #('greenFuse')
        }.
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
