Object subclass: ItemObject [

    | name description |

    ItemObject class >> newWithName: aName description: aDescription [
        ^self new initializeWithName: aName description: aDescription
    ]

    ItemObject >> initializeWithName: aName description: aDescription [
        name := aName.
        description := aDescription.
    ]

    ItemObject >> name [
        ^name
    ]

    ItemObject >> description [
        ^description
    ]

    ItemObject class >> findItem: itemName [
        | itemsMap |
        itemsMap := Dictionary new.

        itemsMap at: 'cigarettes' put: (ItemObject newWithName: 'cigarettes'
        description: 'The pack of cigarettes is old, with some of the edges frayed.',
        'It might be useful to someone who needs a smoke.').

        itemsMap at: 'weird_box' put: (ItemObject newWithName: 'weird_box'
        description: 'A box with a lock that opens with more than 20 digits.',
        'Impossible to open for now, but maybe it will prove useful in the future.').

        itemsMap at: 'harnas' put: (ItemObject newWithName: 'harnas'
        description: 'A can of Harnas beer, cold and seemingly untouched.',
        'It might help you in your interactions with certain characters.').

        itemsMap at: 'kuflowe_mocne' put: (ItemObject newWithName: 'kuflowe_mocne'
        description: 'A bottle of Kuflowe Mocne, a strong beer.',
        'It might be useful for someone who needs a drink.').

        itemsMap at: 'car_keys' put: (ItemObject newWithName: 'car_keys'
        description: 'A set of car keys. They seem to belong to the abandoned car in the parking lot.',
        'Maybe you can unlock it.').

        itemsMap at: 'amulet' put: (ItemObject newWithName: 'amulet'
        description: 'A strange amulet, cold to the touch. It seems to have an otherworldly aura,',
        'as if it carries a hidden power.').

        itemsMap at: 'diary' put: (ItemObject newWithName: 'diary'
        description: 'The diary is yellowed and fragile. Some pages are barely readable,',
        'but one section stands out, scribbled with urgency:',
        '"We have found traces of an entrance near the old oak in the forest.',
        'It must be the cave mentioned in the legends..."').

        itemsMap at: 'notes' put: (ItemObject newWithName: 'notes'
        description: 'The notes are brittle and covered in dust, with several pages missing or torn.',
        '"We found it — the amulet from the old texts.',
        'It is said this amulet is the key to something ancient, hidden deep within the caves."').

        itemsMap at: 'newspaper' put: (ItemObject newWithName: 'newspaper'
        description: 'You start reading it. The headline catches your eye:',
        '"Team of Archaeologists Arrives to Uncover the Secrets of the Ancient Ruins in Town."').

        itemsMap at: 'leaf_with_code' put: (ItemObject newWithName: 'leaf_with_code'
        description: 'The leaf contains a mysterious riddle:',
        '"The code is hidden in two pairs of numbers. The first pair comes from multiplying',
        'three by the digit symbolizing fullness. The second pair is a sequence',
        'you receive from the natural order."').

        itemsMap at: 'engraved_ring' put: (ItemObject newWithName: 'engraved_ring'
        description: 'The ring is engraved with strange symbols and feels unusually heavy in your hand.',
        'It might hold some hidden significance or power.').

        itemsMap at: 'key' put: (ItemObject newWithName: 'key'
        description: 'A small, rusted key. It might be useful for unlocking something.').

        itemsMap at: 'red_fuse' put: (ItemObject newWithName: 'red_fuse'
        description: 'A red fuse, used for electrical circuits. It might be useful for repairing something.').

        itemsMap at: 'green_fuse' put: (ItemObject newWithName: 'green_fuse'
        description: 'A green fuse, used for electrical circuits. It might be useful for repairing something.').

        itemsMap at: 'blue_fuse' put: (ItemObject newWithName: 'blue_fuse'
        description: 'A blue fuse, used for electrical circuits. It might be useful for repairing something.').

        ^itemsMap at: itemName ifAbsent: [nil]
    ]
]
