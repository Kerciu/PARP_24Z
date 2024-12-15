module Introduction where

import Utils

introductionText :: [String]
introductionText = [
    "Welcome to the City of Shadows!",
    "",
    "Your adventure begins in a desolate town filled with strange locations, cryptic symbols, and unsettling figures.",
    "You find yourself at a train station, the clock forever stuck at 3:15 AM.",
    "An eerie silence lingers as the caretaker of the station watches you suspiciously.",
    "Resolving mystery of an ominous cult seem to be central to your quest.",
    "But be warned, the path ahead is fraught with danger and mystery.",
    "As you explore, remember that the choices you make could shape your fate.",
    "Good luck, adventurer, the mystery awaits you!",
    ""
    ]

-- print strings from list in separate lines

printIntroduction :: IO ()
printIntroduction = printLines introductionText