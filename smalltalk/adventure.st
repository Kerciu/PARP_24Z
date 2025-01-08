Object subclass: Game [
    "Instance variables"
    | gameState locationsMap |

    Game class >> initialize [
        "Initialize class variables"
        IntroductionText := #(
            'Welcome to the City of Shadows!'
            ''
            'Your adventure begins in a desolate town filled with strange locations, cryptic symbols, and unsettling figures.'
            'You find yourself at a train station, the clock forever stuck at 3:15 AM.'
            'An eerie silence lingers as the caretaker of the station watches you suspiciously.'
            'Resolving mystery of an ominous cult seems to be central to your quest.'
            'But be warned, the path ahead is fraught with danger and mystery.'
            'As you explore, remember that the choices you make could shape your fate.'
            'Good luck, adventurer, the mystery awaits you!'
            ''
        ).

        InstructionsText := #(
            'Available commands are:'
            ''
            'start                    -- to start the game.'
            'n  s  e  w               -- to go in that direction.'
            'take Object              -- to pick up an object.'
            'drop Object              -- to put down an object.'
            'check Object             -- to check object in inventory.'
            'look                     -- to look around you again.'
            'interact Character       -- to interact with characters.'
            'give Character Object    -- to give object to character.'
            'open Object              -- to open an object.'
            'enter_car                -- to enter the car.'
            'exit_car                 -- to exit the car.'
            'inventory                -- to check inventory contents.'
            'instructions             -- to see this message again.'
            'quit                     -- to end the game and quit.'
            ''
        ).
    ]

    Game class >> introductionText [
        ^IntroductionText
    ]

    Game class >> instructionsText [
        ^InstructionsText
    ]

    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        gameState := GameState new.
        locationsMap := Location createLocations.
        gameState currentLocation: (locationsMap at: 'train_station').
    ]

    handleDirection: direction [
        | nextLocation |
        nextLocation := gameState currentLocation moveTo: direction locationsMap: locationsMap.
        nextLocation ifNotNil: [
            gameState currentLocation: nextLocation.
            self printCurrentLocation.
        ].
    ]

    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
        self printLines: Game introductionText.
    ]

    printInstructions [
        self printLines: Game instructionsText.
    ]

    printCurrentLocation [
        Transcript show: gameState currentLocation description; cr.
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
        gameState lookAround.
    ]

    handleTake: object [
        gameState takeItem: object.
    ]

    handleDrop: object [
        gameState dropItem: object.
    ]

    handleCheck: object [
        gameState checkItem: object.
    ]

    handleGive: character to: object [
        gameState giveItem: character to: object.
    ]

    handleInteract: character [
        gameState interactWith: character.
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
        gameState showInventory.
    ]

    handleInstructions [
        self printInstructions.
    ]

    handleQuit [
        gameState updateGameOver.
        Transcript show: 'Quitting the game. Goodbye!'; cr.
    ]

    "Run the game."
    run [
        | cmd |
        self printIntroduction.
        self printInstructions.

        "Game loop."
        [gameState gameOver not] whileTrue: [
            cmd := self readCommand.
            self parseCommand: cmd.
        ].
    ]

]

"Run the game"
Game initialize.
Game new run.