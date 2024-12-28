"The germ of a text adventure game
Marcin Szlenk 2024'

'Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."

Object subclass: Game [
    | isGameOver gameState |

    introductionText := #(
        'Welcome to the City of Shadows!',
        '',
        'Your adventure begins in a desolate town filled with strange locations, cryptic symbols, and unsettling figures.',
        'You find yourself at a train station, the clock forever stuck at 3:15 AM.',
        'An eerie silence lingers as the caretaker of the station watches you suspiciously.',
        'Resolving mystery of an ominous cult seem to be central to your quest.',
        'But be warned, the path ahead is fraught with danger and mystery.',
        'As you explore, remember that the choices you make could shape your fate.',
        'Good luck, adventurer, the mystery awaits you!',
        ''
    ).

    instructionsText := #(
        'Available commands are:',
        '',
        'start                    -- to start the game.',
        'n  s  e  w               -- to go in that direction.',
        'take Object              -- to pick up an object.',
        'drop Object              -- to put down an object.',
        'check Object             -- to check object in inventory.',
        'look                     -- to look around you again.',
        'interact Character       -- to interact with characters.',
        'give Character Object    -- to give object to character.',
        'open Object              -- to open an object.',
        'enter_car                -- to enter the car.',
        'exit_car                 -- to exit the car.',
        'inventory                -- to check inventory contents.',
        'instructions             -- to see this message again.',
        'quit                     -- to end the game and quit.',
        ''
    ).

    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        isGameOver := false
        gameState := GameState new.
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
       self printLines: Game introductionText.
    ]

    printInstructions [
        self printLines: Game instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    parseCommand: cmd [
        | tokens command args |
        tokens := cmd tokenize: ' '. "Split the input into tokens."
        command := tokens first ifNil: [''].
        args := tokens allButFirst.

        (command = 'start') ifTrue: [^self handleStart].
        (command = 'n' or: [command = 's' or: [command = 'w' or: [command = 'e']]]) ifTrue: [
            ^self handleDirection: command
        ].
        (command = 'take') ifTrue: [^self handleTake: args first].
        (command = 'drop') ifTrue: [^self handleDrop: args first].
        (command = 'check') ifTrue: [^self handleCheck: args first].
        (command = 'look') ifTrue: [^self handleLook].
        (command = 'interact') ifTrue: [^self handleInteract: args first].
        (command = 'give') ifTrue: [^self handleGive: args first to: args second].
        (command = 'open') ifTrue: [^self handleOpen: args first].
        (command = 'enter_car') ifTrue: [^self handleEnterCar].
        (command = 'exit_car') ifTrue: [^self handleExitCar].
        (command = 'inventory') ifTrue: [^self handleInventory].
        (command = 'instructions') ifTrue: [^self handleInstructions].
        (command = 'quit') ifTrue: [^self handleQuit].

        self printLines: #('Unknown command.' '').
    ]

    handleStart [
        gameState initialize.
        self printIntroduction.
        self printInstructions.
    ]

    handleLook [
        Transcript show: 'Not implemented yet: TODO'.
    ]

    handleTake: object [
        Transcript show: 'Not implemented yet: You take ', object; cr.
    ]

    handleDrop: object [
        Transcript show: 'Not implemented yet: You drop ', object; cr.
    ]

    handleCheck: object [
        Transcript show: 'Not implemented yet: Checking ', object; cr.
    ]

    handleGive: character to: object [
        Transcript show: 'Not implemented yet: You give ', object, ' to ', character; cr.
    ]

    handleInteract: character [
        Transcript show: 'Not implemented yet: You interact with ', character; cr.
    ]

    handleOpen: object [
        Transcript show: 'Not implemented yet: You open ', object; cr.
    ]

    handleEnterCar [
        Transcript show: 'Not implemented yet: You enter the car.'; cr.
    ]

    handleExitCar [
        Transcript show: 'Not implemented yet: You exit the car.'; cr.
    ]

    handleInventory [
        Transcript show: 'Not implemented yet: TODO'; cr.
    ]

    handleInstructions [
        self printInstructions.
    ]

    handleQuit [
        isGameOver := true.
        Transcript show: 'Quitting the game. Goodbye!'; cr.
    ]

    "Run the game."
    run [
        | cmd isUnknown |

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [isGameOver] whileFalse: [
            isUnknown := true.
            cmd := self readCommand.

            cmd = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].

            isUnknown ifTrue: [
                self printLines: #('Unknown command.' '')
            ]
        ]
    ]

]

Game new run.
